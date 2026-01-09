const std = @import("std");
const ScanError = @import("error.zig").ScanError;

pub const TokenType = enum {
    // Single-character tokens
    LEFT_PAREN, RIGHT_PAREN, 
    LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS,
    SEMICOLON, ARROW, SLASH, STAR,
    // One or two character tokens
    BANG, BANG_EQUAL, 
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,
    // Literals
    IDENTIFIER, STRING, NUMBER,
    // Keywords
    AND, CLASS, ELSE, FALSE,
    FOR, FUN, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS,
    TRUE, VAR, CONST, WHILE,
    SWITCH, DEFAULT, CONTINUE,

    ERROR, EOF
};

pub const Token = struct {
    ttype: TokenType,
    token: []const u8, 
    line: usize,

    pub fn init(ttype: TokenType, token: []const u8, line: usize) Token {
        return .{
            .ttype = ttype, 
            .token = token,
            .line = line
        };
    }

    pub fn errorToken(message: []const u8, line: usize) Token {
        _ = message;
        return .{
            .ttype = .ERROR,
            .token = "error",
            .line = line
        };
    }

    pub fn print(self: Token) void {
        std.debug.print("[ {any}, {s}, {d} ]\n", .{self.ttype, self.token, self.line});
    }
};


pub const Scanner = struct {
    start: [*]const u8,
    current: [*]const u8,
    line: usize,
    remaining: usize,

    const Self = @This();

    pub fn init(source: []const u8) Scanner {
        return .{
            .start = source[0..source.len].ptr,
            .current = source[0..source.len].ptr,
            .line = 0,
            // NOTE: need to include this to handle no sentinel for the repl
            .remaining = source.len,
        };
    }

    pub fn scanToken(self: *Self) ScanError!Token {
        self.skipWhitespace();
        self.update();
        if (self.atEnd()) return Token.init(.EOF, "EOF", self.line);
        const char = self.advance();

        if (Scanner.isDigit(char)) return self.number();
        if (Scanner.isAlpha(char)) return self.identifier();

        const ttype: TokenType = ttype: switch(char) {
            '(' => .LEFT_PAREN,
            ')' => .RIGHT_PAREN,
            '{' => .LEFT_BRACE,
            '}' => .RIGHT_BRACE,
            ';' => .SEMICOLON,
            ',' => .COMMA,
            '.' => .DOT,
            '-' => .MINUS,
            '+' => .PLUS,
            '/' => .SLASH,             
            '*' => .STAR,
            '"' => return self.string(),
            '!' => {
                if (self.match('=')) {
                    break :ttype .BANG_EQUAL; 
                } else {
                    break :ttype .BANG; 
                }
            },
            '=' => {
                if (self.match('=')) {
                    break :ttype .EQUAL_EQUAL;
                } else if (self.match('>')) {
                    break :ttype .ARROW;
                } else {
                    break :ttype .EQUAL;
                }
            },
            '>' => {
                if (self.match('=')) {
                    break :ttype .GREATER_EQUAL;
                } else {
                    break :ttype .GREATER;
                }
            },
            '<' => {
                if (self.match('=')) {
                    break :ttype .LESS_EQUAL;
                } else {
                    break :ttype .LESS;
                }
            },
            else => undefined,
        };

        return Token.init(ttype, self.getToken(), self.line);
    }

    fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn isAlpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or 
            (char >= 'A' and char <= 'Z') or
            char == '_';
    }

    fn getToken(self: *Self) []const u8 {
        defer self.update();
        return self.start[0..self.current - self.start];
    }

    fn peek(self: *Self) u8 {
        if (self.atEnd()) return 0; 
        return self.current[0];
    }

    fn identifier(self: *Self) Token {
        while (Scanner.isAlpha(self.peek()) or Scanner.isDigit(self.peek())) _ = self.advance();
        const ttype = self.identifierType();
        return Token.init(ttype, self.getToken(), self.line);
    }

    fn identifierType(self: *Self) TokenType {
        return switch (self.start[0]) {
            'a' => self.checkKeyword(1, "nd", .AND),
            'e' => self.checkKeyword(1, "lse", .ELSE),
            'i' => self.checkKeyword(1, "f", .IF),
            'd' => self.checkKeyword(1, "efault", .DEFAULT),
            'n' => self.checkKeyword(1, "il", .NIL),
            'o' => self.checkKeyword(1, "r", .OR),
            'p' => self.checkKeyword(1, "rint", .PRINT),
            'r' => self.checkKeyword(1, "eturn", .RETURN),
            's' => {
                if (self.current - self.start <= 1) {
                    return .IDENTIFIER;
                }
                return switch (self.start[1]) {
                    'w' => self.checkKeyword(2, "itch", .SWITCH),
                    'u' => self.checkKeyword(2, "per", .SUPER),
                    else => .IDENTIFIER,
                };
            },
            'v' => self.checkKeyword(1, "ar", .VAR),
            'w' => self.checkKeyword(1, "hile", .WHILE),
            'c' => {
                if (self.current - self.start <= 1) {
                    return .IDENTIFIER;
                }
                return switch (self.start[1]) {
                    'l' => self.checkKeyword(2, "ass", .CLASS),
                    'o' => self.checkKeywordCo(),
                    else => .IDENTIFIER,
                };
            },
            'f' => { 
                if (self.current - self.start <= 1) {
                    return .IDENTIFIER;
                }
                return switch (self.start[1]) {
                    'a' => self.checkKeyword(2, "lse", .FALSE),
                    'o' => self.checkKeyword(2, "r", .FOR),
                    'u' => self.checkKeyword(2, "n", .FUN),
                    else => .IDENTIFIER,
                };
            },
            't' => {
                if (self.current - self.start <= 1) {
                    return .IDENTIFIER;
                }
                return switch (self.start[1]) {
                    'h' => self.checkKeyword(2, "is", .THIS),
                    'r' => self.checkKeyword(2, "ue", .TRUE),
                    else => .IDENTIFIER,
                };
            },
            else => .IDENTIFIER
        };
    }

    fn checkKeywordCo(self: *Self) TokenType {
        if (self.current - self.start <= 2) {
            return .IDENTIFIER;
        }
        return switch (self.start[2]) {
            'n' => {
                if (self.current - self.start <= 3) {
                    return .IDENTIFIER;
                }
                return switch (self.start[3]) {
                    's' => self.checkKeyword(3, "st", .CONST),
                    't' => self.checkKeyword(3, "tinue", .CONTINUE),
                    else => .IDENTIFIER,
                };
            },
            else => .IDENTIFIER,
        };
    }

    fn checkKeyword(self: *Self, start: usize, str: []const u8, ttype: TokenType) TokenType {
        if ((self.current - self.start) - start > str.len) {
            return .IDENTIFIER;
        }
        if (std.mem.eql(u8, self.start[start..self.current-self.start], str)) {
            return ttype;
        }
        return .IDENTIFIER;
    }

    fn number(self: *Self) Token {
        while (Scanner.isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and Scanner.isDigit(self.peekNext())) {
            _ = self.advance();

            while (Scanner.isDigit(self.peek())) _ = self.advance();
        }

        return Token.init(.NUMBER, self.getToken(), self.line);
    }

    fn string(self: *Self) ScanError!Token {
        while (self.peek() != '"' and !self.atEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.atEnd()) return ScanError.UnterminatedString;
        
        _ = self.advance();
        return Token.init(.STRING, self.getToken(), self.line);
    }

    fn peekNext(self: *Self) u8 {
        if (self.atEnd()) return 0; 
        return self.current[1];
    }

    /// Only advance the multi item pointer if there is a match
    fn match(self: *Self, char: u8) bool {
        if (self.atEnd()) return false; 
        if (self.current[0] != char) return false;
        self.remaining -= 1;
        self.current += 1;
        return true;
    }


    fn atEnd(self: *Self) bool {
        return self.remaining == 0 or self.current[0] == 0;
    }

    fn advance(self: *Self) u8 {
        defer self.current += 1;
        self.remaining = @max(self.remaining - 1, 0);
        return self.current[0];
    }

    fn update(self: *Self) void {
        self.start = self.current;
    }


    fn skipWhitespace(self: *Self) void {
        while (true) {
            _ = switch(self.peek()) {
                ' ', '\r', '\t' => self.advance(),
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.atEnd()) { 
                            _ = self.advance(); 
                        }

                        if (self.peek() == '\n') {
                            _ = self.advance();
                        }
                    }
                    return;
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                    return;
                },
                else => return,
            };
        }

    }
};

// TODO: implement snapshot testing for these.
test "scan token" {
    const single_line_source: []const u8 = "( ) { } ; , . - + / * ! != < <= > >= = == // don't return this comment\n( / ( ) \"hello\" + +=\ntrue false fun if else nil or super return print var while and class whil close c a t const switch default continue";
    const allocator = std.testing.allocator;
    const source = try allocator.alloc(u8, single_line_source.len);
    defer allocator.free(source);
    @memcpy(source, single_line_source);
    
    var scanner = Scanner.init(source);
    while (true) {
        const token = try scanner.scanToken();
        token.print();
        if (token.ttype == .EOF) {
            break;
        }
    }
}

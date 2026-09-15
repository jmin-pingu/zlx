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
            .line = 1,
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
                },
                else => return,
            };
        }

    }
};

// TODO: implement snapshot testing for these.
test "scan tokens" {
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

const testing = std.testing;

const ExpectedToken = struct { ttype: TokenType, text: []const u8, line: ?usize = null };

fn expectTokens(source: []const u8, expected: []const ExpectedToken) !void {
    var scanner = Scanner.init(source);
    for (expected) |want| {
        const got = try scanner.scanToken();
        try testing.expectEqual(want.ttype, got.ttype);
        try testing.expectEqualStrings(want.text, got.token);
        if (want.line) |line| try testing.expectEqual(line, got.line);
    }
    const eof = try scanner.scanToken();
    try testing.expectEqual(TokenType.EOF, eof.ttype);
}

test "scanner single character tokens" {
    try expectTokens("(){};,.-+/*", &.{
        .{ .ttype = .LEFT_PAREN, .text = "(" },
        .{ .ttype = .RIGHT_PAREN, .text = ")" },
        .{ .ttype = .LEFT_BRACE, .text = "{" },
        .{ .ttype = .RIGHT_BRACE, .text = "}" },
        .{ .ttype = .SEMICOLON, .text = ";" },
        .{ .ttype = .COMMA, .text = "," },
        .{ .ttype = .DOT, .text = "." },
        .{ .ttype = .MINUS, .text = "-" },
        .{ .ttype = .PLUS, .text = "+" },
        .{ .ttype = .SLASH, .text = "/" },
        .{ .ttype = .STAR, .text = "*" },
    });
}

test "scanner one or two character tokens" {
    try expectTokens("! != = == => > >= < <=", &.{
        .{ .ttype = .BANG, .text = "!" },
        .{ .ttype = .BANG_EQUAL, .text = "!=" },
        .{ .ttype = .EQUAL, .text = "=" },
        .{ .ttype = .EQUAL_EQUAL, .text = "==" },
        .{ .ttype = .ARROW, .text = "=>" },
        .{ .ttype = .GREATER, .text = ">" },
        .{ .ttype = .GREATER_EQUAL, .text = ">=" },
        .{ .ttype = .LESS, .text = "<" },
        .{ .ttype = .LESS_EQUAL, .text = "<=" },
    });
    try expectTokens("a==b=>c", &.{
        .{ .ttype = .IDENTIFIER, .text = "a" },
        .{ .ttype = .EQUAL_EQUAL, .text = "==" },
        .{ .ttype = .IDENTIFIER, .text = "b" },
        .{ .ttype = .ARROW, .text = "=>" },
        .{ .ttype = .IDENTIFIER, .text = "c" },
    });
}

test "scanner keywords" {
    const keywords = [_]struct { []const u8, TokenType }{
        .{ "and", .AND },       .{ "class", .CLASS },   .{ "else", .ELSE },       .{ "false", .FALSE },
        .{ "for", .FOR },       .{ "fun", .FUN },       .{ "if", .IF },           .{ "nil", .NIL },
        .{ "or", .OR },         .{ "print", .PRINT },   .{ "return", .RETURN },   .{ "super", .SUPER },
        .{ "this", .THIS },     .{ "true", .TRUE },     .{ "var", .VAR },         .{ "const", .CONST },
        .{ "while", .WHILE },   .{ "switch", .SWITCH }, .{ "default", .DEFAULT }, .{ "continue", .CONTINUE },
    };
    for (keywords) |kw| {
        try expectTokens(kw[0], &.{.{ .ttype = kw[1], .text = kw[0] }});
    }
}

test "scanner keyword prefixes and extensions are identifiers" {
    const idents = [_][]const u8{
        "a",    "an",     "ands",   "c",         "co",   "con",    "cons", "constant", "cont",
        "continued", "f", "fa",     "fo",        "forx", "fu",     "s",    "su",       "sw",
        "switc", "t",     "th",     "tr",        "whil", "close",  "nile", "printer",  "_",
        "_x1",  "camelCase", "x2y", "iff",       "orb",  "variable",
    };
    for (idents) |ident| {
        try expectTokens(ident, &.{.{ .ttype = .IDENTIFIER, .text = ident }});
    }
}

test "scanner numbers" {
    try expectTokens("0 42 3.14 1. .5 12abc", &.{
        .{ .ttype = .NUMBER, .text = "0" },
        .{ .ttype = .NUMBER, .text = "42" },
        .{ .ttype = .NUMBER, .text = "3.14" },
        .{ .ttype = .NUMBER, .text = "1" },
        .{ .ttype = .DOT, .text = "." },
        .{ .ttype = .DOT, .text = "." },
        .{ .ttype = .NUMBER, .text = "5" },
        .{ .ttype = .NUMBER, .text = "12" },
        .{ .ttype = .IDENTIFIER, .text = "abc" },
    });
}

test "scanner strings keep their quotes and track embedded newlines" {
    try expectTokens("\"hello\" \"\" \"multi\nline\" after", &.{
        .{ .ttype = .STRING, .text = "\"hello\"", .line = 1 },
        .{ .ttype = .STRING, .text = "\"\"", .line = 1 },
        .{ .ttype = .STRING, .text = "\"multi\nline\"", .line = 2 },
        .{ .ttype = .IDENTIFIER, .text = "after", .line = 2 },
    });
}

test "scanner unterminated string is an error" {
    var scanner = Scanner.init("\"never closed");
    try testing.expectError(ScanError.UnterminatedString, scanner.scanToken());
}

test "scanner skips whitespace and line comments" {
    try expectTokens("  \t\r a // comment here\nb // trailing comment without newline", &.{
        .{ .ttype = .IDENTIFIER, .text = "a" },
        .{ .ttype = .IDENTIFIER, .text = "b" },
    });
    try expectTokens("a / b", &.{
        .{ .ttype = .IDENTIFIER, .text = "a" },
        .{ .ttype = .SLASH, .text = "/" },
        .{ .ttype = .IDENTIFIER, .text = "b" },
    });
}

test "scanner counts lines across newlines" {
    try expectTokens("a\n\nb\n  c", &.{
        .{ .ttype = .IDENTIFIER, .text = "a", .line = 1 },
        .{ .ttype = .IDENTIFIER, .text = "b", .line = 3 },
        .{ .ttype = .IDENTIFIER, .text = "c", .line = 4 },
    });
}

test "scanner line comment newline advances the line counter" {
    try expectTokens("a // note\nb", &.{
        .{ .ttype = .IDENTIFIER, .text = "a", .line = 1 },
        .{ .ttype = .IDENTIFIER, .text = "b", .line = 2 },
    });
}

test "scanner empty and whitespace-only sources yield EOF" {
    try expectTokens("", &[_]ExpectedToken{});
    try expectTokens("  \n\t ", &[_]ExpectedToken{});
    var scanner = Scanner.init("");
    const eof = try scanner.scanToken();
    try testing.expectEqualStrings("EOF", eof.token);
    try testing.expectEqual(@as(usize, 1), eof.line);
    try testing.expectEqual(TokenType.EOF, (try scanner.scanToken()).ttype);
}

test "scanner token constructors" {
    const tok = Token.init(.NUMBER, "1", 4);
    try testing.expectEqual(TokenType.NUMBER, tok.ttype);
    try testing.expectEqualStrings("1", tok.token);
    try testing.expectEqual(@as(usize, 4), tok.line);

    const err = Token.errorToken("ignored", 9);
    try testing.expectEqual(TokenType.ERROR, err.ttype);
    try testing.expectEqualStrings("error", err.token);
    try testing.expectEqual(@as(usize, 9), err.line);
}

test "scanner tokenizes a realistic declaration" {
    try expectTokens("fun add(a, b) { return a + b; }", &.{
        .{ .ttype = .FUN, .text = "fun" },
        .{ .ttype = .IDENTIFIER, .text = "add" },
        .{ .ttype = .LEFT_PAREN, .text = "(" },
        .{ .ttype = .IDENTIFIER, .text = "a" },
        .{ .ttype = .COMMA, .text = "," },
        .{ .ttype = .IDENTIFIER, .text = "b" },
        .{ .ttype = .RIGHT_PAREN, .text = ")" },
        .{ .ttype = .LEFT_BRACE, .text = "{" },
        .{ .ttype = .RETURN, .text = "return" },
        .{ .ttype = .IDENTIFIER, .text = "a" },
        .{ .ttype = .PLUS, .text = "+" },
        .{ .ttype = .IDENTIFIER, .text = "b" },
        .{ .ttype = .SEMICOLON, .text = ";" },
        .{ .ttype = .RIGHT_BRACE, .text = "}" },
    });
}

test "scanner does not interpret escape sequences" {
    const newline =
        \\"a\nb"
    ;
    try testing.expectEqual(@as(usize, 6), newline.len);
    try expectTokens(newline, &.{.{ .ttype = .STRING, .text = newline, .line = 1 }});

    const others = [_][]const u8{
        \\"a\tb"
        ,
        \\"a\\b"
        ,
        \\"a\0b"
        ,
        \\"a\u{1}b"
        ,
    };
    for (others) |source| {
        try expectTokens(source, &.{.{ .ttype = .STRING, .text = source }});
    }
}

test "scanner counts an escape sequence as two characters" {
    const source =
        \\"\n"
    ;
    var scanner = Scanner.init(source);
    const token = try scanner.scanToken();
    try testing.expectEqual(@as(usize, 4), token.token.len);
    try testing.expectEqual(@as(u8, '\\'), token.token[1]);
    try testing.expectEqual(@as(u8, 'n'), token.token[2]);
    try testing.expectEqual(@as(usize, 1), token.line);
    try testing.expect(std.mem.indexOfScalar(u8, token.token, '\n') == null);
}

test "scanner backslash does not escape a closing quote" {
    const source =
        \\"a\"b"
    ;
    const expected =
        \\"a\"
    ;
    var scanner = Scanner.init(source);

    const string_token = try scanner.scanToken();
    try testing.expectEqual(TokenType.STRING, string_token.ttype);
    try testing.expectEqualStrings(expected, string_token.token);

    const ident = try scanner.scanToken();
    try testing.expectEqual(TokenType.IDENTIFIER, ident.ttype);
    try testing.expectEqualStrings("b", ident.token);

    try testing.expectError(ScanError.UnterminatedString, scanner.scanToken());
}

test "scanner treats a real newline inside a string as content" {
    const source = "\"a\nb\" c";
    try expectTokens(source, &.{
        .{ .ttype = .STRING, .text = "\"a\nb\"", .line = 2 },
        .{ .ttype = .IDENTIFIER, .text = "c", .line = 2 },
    });
}

test "scanner keeps control characters inside a string" {
    const source = "\"a\x01\x1b\x07\x7f\x0b\x0cb\" x";
    try expectTokens(source, &.{
        .{ .ttype = .STRING, .text = "\"a\x01\x1b\x07\x7f\x0b\x0cb\"", .line = 1 },
        .{ .ttype = .IDENTIFIER, .text = "x", .line = 1 },
    });

    var scanner = Scanner.init(source);
    const token = try scanner.scanToken();
    try testing.expectEqual(@as(usize, 10), token.token.len);
    try testing.expectEqualSlices(u8, &[_]u8{ 0x22, 'a', 0x01, 0x1b, 0x07, 0x7f, 0x0b, 0x0c, 'b', 0x22 }, token.token);
}

test "scanner treats tab and carriage return as whitespace" {
    try expectTokens("\ta\r\tb\r", &.{
        .{ .ttype = .IDENTIFIER, .text = "a", .line = 1 },
        .{ .ttype = .IDENTIFIER, .text = "b", .line = 1 },
    });
    try expectTokens("\r\n\t", &[_]ExpectedToken{});
}

test "scanner stops at a NUL byte" {
    var scanner = Scanner.init("a\x00b");
    const first = try scanner.scanToken();
    try testing.expectEqual(TokenType.IDENTIFIER, first.ttype);
    try testing.expectEqualStrings("a", first.token);
    try testing.expectEqual(TokenType.EOF, (try scanner.scanToken()).ttype);

    var unterminated = Scanner.init("\"a\x00b\"");
    try testing.expectError(ScanError.UnterminatedString, unterminated.scanToken());
}

test "scanner reports an unrecognized character as an error token" {
    const unhandled = [_][]const u8{ "\x01", "\x0b", "\x0c", "\x1b", "\x7f", "@", "#", "$", "%", "^", "&", "~", "?", ":", "[", "]" };
    for (unhandled) |source| {
        var scanner = Scanner.init(source);
        try testing.expectEqual(TokenType.ERROR, (try scanner.scanToken()).ttype);
    }
}

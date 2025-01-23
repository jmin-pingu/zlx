const std = @import("std");

const ArrayList = std.ArrayList;
const StaticStringMap = std.StaticStringMap;

const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;

const err_lib = @import("error.zig");
const Error = err_lib.Error;

// TODO: need to implement error printing with syntax errors
 
// Creating a comptime known hashmap!
const kvs_list = [_]struct { []const u8, TokenType }{
    .{ "and", TokenType.AND},
    .{ "class", TokenType.CLASS},
    .{ "else", TokenType.ELSE},
    .{ "false", TokenType.FALSE},
    .{ "for", TokenType.FOR},
    .{ "fun", TokenType.FUN},
    .{ "if", TokenType.IF},
    .{ "nil", TokenType.NIL},
    .{ "or", TokenType.OR},
    .{ "print", TokenType.PRINT},
    .{ "return", TokenType.RETURN},
    .{ "super", TokenType.SUPER},
    .{ "this", TokenType.THIS},
    .{ "true", TokenType.TRUE},
    .{ "var", TokenType.VAR},
    .{ "while", TokenType.WHILE},
};

const keywords = StaticStringMap(TokenType).initComptime(kvs_list);

pub const Scanner = struct {
    source: []const u8,
    tokens: ArrayList(Token),
    start: u32 = 0,
    current: u32 = 0,
    line: u32 = 1,
    allocator: std.mem.Allocator,
    // NOTE: start v. current
    // start: the START position of the current token
    // current: the CURRENT position in the current token

    pub fn new(source: []const u8, allocator: std.mem.Allocator) Scanner {
        return Scanner{
            .source = source,
            // NOTE: do not need to worry about freeing since we are using an ArenaAllocator (we should free at the highest level)
            .tokens = ArrayList(Token).init(allocator),
            .allocator = allocator
        };
    }

    pub fn scanTokens(self: *Scanner) Error!ArrayList(Token) {
        while ( !self.reachedEnd() ) {
            self.start = self.current;
            // TODO: implement SyntaxError handling
            try self.scanToken();
        }
        self.tokens.append(Token.new(TokenType.EOF, "", null, self.line)) catch unreachable;
        return self.tokens;
    }

    fn reachedEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }
 
    fn scanToken(self: *Scanner) Error!void {
        const c = self.advance();
        switch (c) {
            // single char
            '(' => self.addToken(TokenType.LEFT_PAREN, null),
            ')' => self.addToken(TokenType.RIGHT_PAREN, null),
            '{' => self.addToken(TokenType.LEFT_BRACE, null),
            '}' => self.addToken(TokenType.RIGHT_BRACE, null),
            ',' => self.addToken(TokenType.COMMA, null),
            '.' => self.addToken(TokenType.DOT, null),
            '-' => self.addToken(TokenType.MINUS, null),
            '+' => self.addToken(TokenType.PLUS, null),
            ';' => self.addToken(TokenType.SEMICOLON, null),
            '*' => self.addToken(TokenType.STAR, null),
            // one OR two character tokens
            '!' => self.addToken(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG, null),
            '=' => self.addToken(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL, null),
            '<' => self.addToken(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS, null),
            '>' => self.addToken(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER, null),
            // Longer lexemes: require some lookahead
            '/' => {
                if (self.match('/')) {
                    // One line comment match
                    while (self.peek() != '\n' and !self.reachedEnd()) _ = self.advance();
                } else if (self.match('*')) { 
                    // Multiline comment match
                    while (self.peek() != '*' and self.peekNext() != '/' and !self.reachedEnd()) _ = self.advance();
                    // Need to advance twice to go beyond "*/"
                    _ = self.advance();
                    _ = self.advance();
                } else {
                    self.addToken(TokenType.SLASH, null);
                }
            },            
            '"' => self.string() catch |err| return err,
            // NULL lexemes
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,
            else => {
                if (std.ascii.isDigit(c)) {
                    self.number(); 
                } else if (std.ascii.isAlphabetic(c) or c == '_') {
                    try self.identifier();
                } else {
                    const message = "syntax error";
                    return err_lib.error_msg(self.line, message, Error.SyntaxError, self.allocator);
                }
            }
        }
    }

    fn identifier(self: *Scanner) Error!void {
        while (self.peek() == '_' or std.ascii.isAlphanumeric(self.peek())) _ = self.advance();
        const text = self.source[self.start..self.current];

        if (keywords.get(text)) |ttype| {
            self.addToken(ttype, null);
        } else {
            const copied_text = try self.allocator.alloc(u8, text.len);
            @memcpy(copied_text, text);
            self.addToken(TokenType.IDENTIFIER, copied_text);
        }
    }

    fn number(self: *Scanner) void {
        while (std.ascii.isDigit(self.peek())) _ = self.advance();

        // fraction check
        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) _ = self.advance();

        while (std.ascii.isDigit(self.peek())) _ = self.advance();

        const value = self.source[self.start..self.current];
        self.addToken(TokenType.NUMBER, value);
    }
    
    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return '\n';
        return self.source[self.current + 1];
    }

    fn string(self: *Scanner) Error!void {
        while (self.peek() != '"' and !self.reachedEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.reachedEnd()) {
            const message = "failure to terminate string";
            return err_lib.error_msg(self.line, message, Error.SyntaxError, self.allocator);
        }

        _ = self.advance();

        const value = self.source[self.start+1..self.current-1];
        const copied_value = try self.allocator.alloc(u8, value.len);
        @memcpy(copied_value, value);

        self.addToken(TokenType.STRING, copied_value);
    }
    // NOTE: one char lookahead
    fn peek(self: *Scanner) u8 {
        // TODO: double-check whether the logic is good 
        if (self.reachedEnd()) return '\n';
        return self.source[self.current];
    }

    fn addToken(self: *Scanner, ttype: TokenType, literal: ?[]const u8) void {
        self.tokens.append(Token.new(ttype, self.source[self.start..self.current], literal, self.line)) catch unreachable;
    }

    // key helper function: updates the current position and returns the character
    fn advance (self: *Scanner) u8 {
        defer self.current += 1;
        return self.source[self.current];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.reachedEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }
};

test "init_test" {
    const source = "testing initialization!\n";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const scanner = Scanner.new(source, allocator);
    try std.testing.expectEqual(@as(u32, 1), scanner.line);
    try std.testing.expectEqual(@as(u32, 0), scanner.current);
    try std.testing.expectEqual(@as(u32, 0), scanner.start);
}

test "syntax_error1_test" {
    const source = "@\n";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    std.debug.print("syntax_error1_test:\n", .{});
    try std.testing.expectError(Error.SyntaxError, scanner.scanTokens());
    std.debug.print("\n", .{});
}

test "syntax_error2_test" {
    const source = "\"hello world\n";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    std.debug.print("syntax_error2_test:\n", .{});
    try std.testing.expectError(Error.SyntaxError, scanner.scanTokens());
    std.debug.print("\n", .{});
}

test "no_error1_test" {
    const source =  
    \\// this is a comment
    \\(( )) {}  // grouping stuff
    \\!*+=-<> <= >= == //operators
    ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    const parsed = try scanner.scanTokens();
    std.debug.print("no_error1_test: parsed tokens\n", .{});
    for (parsed.items) |token| {
        var new_token = token;
        std.debug.print("\t{s}", .{new_token.to_string(allocator)});
    }
    std.debug.print("\n", .{});
}

test "string_test" {
    const source = "\"hello world\"";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    const parsed = try scanner.scanTokens();
    std.debug.print("string_test: parsed tokens\n", .{});
    for (parsed.items) |token| {
        var new_token = token;
        std.debug.print("\t{s}", .{new_token.to_string(allocator)});
    }
    std.debug.print("\n", .{});
}

test "number1_test" {
    const source = "123456789";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    const parsed = try scanner.scanTokens();
    std.debug.print("number1_test: parsed tokens\n", .{});
    for (parsed.items) |token| {
        var new_token = token;
        std.debug.print("\t{s}", .{new_token.to_string(allocator)});
    }
    std.debug.print("\n", .{});
}

test "number2_test" {
    const source = "12345.6789";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    const parsed = try scanner.scanTokens();
    std.debug.print("number2_test: parsed tokens\n", .{});
    for (parsed.items) |token| {
        var new_token = token;
        std.debug.print("\t{s}", .{new_token.to_string(allocator)});
    }
    std.debug.print("\n", .{});
}

test "identifier_test" {
    const source = "and class else false for fun if nil or print return super this true var while identifier";
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    const parsed = try scanner.scanTokens();
    std.debug.print("identifier_test: parsed tokens\n", .{});
    for (parsed.items) |token| {
        var new_token = token;
        std.debug.print("\t{s}", .{new_token.to_string(allocator)});
    }
    std.debug.print("\n", .{});
}

test "nested_comment_test" {
    const source = 
        \\/* this is a test
        \\ this is a test
        \\ i am continuing to write */
        \\ var str = "hello world!"
        ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    const parsed = try scanner.scanTokens();
    std.debug.print("nested_comment_test: parsed tokens\n", .{});
    for (parsed.items) |token| {
        var new_token = token;
        std.debug.print("\t{s}", .{new_token.to_string(allocator)});
    }
    std.debug.print("\n", .{});
}

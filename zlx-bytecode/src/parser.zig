const std = @import("std");
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const Scanner = @import("scanner.zig").Scanner;
const ParseError = @import("error.zig").ParseError;
const errorAt = @import("error.zig").errorAt;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;

pub const Parser = struct {
    current: Token,
    previous: Token,
    scanner: *Scanner,
    const Self = @This();

    pub fn init(source: []const u8, allocator: std.mem.Allocator) ParseError!Self {
        const scanner = try allocator.create(Scanner);
        scanner.* = Scanner.init(source);
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self.scanner);
    }

    pub fn advance(self: *Self) ParseError!void {
        self.previous = self.current;
        while (true) {
            self.current = try self.scanner.scanToken();
            if (self.current.ttype != .ERROR) {
                return;
            }

            try self.errorAtCurrent(ParseError.ConsumedErrorToken);
        }
    }

    pub fn consume(self: *Self, ttype: TokenType, err: ParseError) ParseError!void {
        if (self.current.ttype == ttype) {
            try self.advance();
            return;
        }
        return self.errorAtCurrent(err);
    }

    // Error handling
    pub fn errorAtCurrent(self: Self, err: ParseError) ParseError!void {
        return errorAt(&self.current, err);
    }

    pub fn errorAtPrevious(self: Self, err: ParseError) ParseError!void {
        return errorAt(&self.previous, err);
    }
};


const testing = std.testing;

test "parser advance moves current into previous" {
    var parser = try Parser.init("1 + 2", testing.allocator);
    defer parser.deinit(testing.allocator);

    try parser.advance();
    try testing.expectEqual(TokenType.NUMBER, parser.current.ttype);
    try testing.expectEqualStrings("1", parser.current.token);

    try parser.advance();
    try testing.expectEqual(TokenType.NUMBER, parser.previous.ttype);
    try testing.expectEqual(TokenType.PLUS, parser.current.ttype);

    try parser.advance();
    try testing.expectEqual(TokenType.PLUS, parser.previous.ttype);
    try testing.expectEqualStrings("2", parser.current.token);

    try parser.advance();
    try testing.expectEqual(TokenType.EOF, parser.current.ttype);
}

test "parser consume advances only on a matching token" {
    var parser = try Parser.init("( )", testing.allocator);
    defer parser.deinit(testing.allocator);
    try parser.advance();

    try parser.consume(.LEFT_PAREN, ParseError.TODO);
    try testing.expectEqual(TokenType.LEFT_PAREN, parser.previous.ttype);
    try testing.expectEqual(TokenType.RIGHT_PAREN, parser.current.ttype);

    try testing.expectError(ParseError.NoClosingSemicolon, parser.consume(.SEMICOLON, ParseError.NoClosingSemicolon));
    try testing.expectEqual(TokenType.RIGHT_PAREN, parser.current.ttype);
    try testing.expectEqual(TokenType.LEFT_PAREN, parser.previous.ttype);
}

test "parser surfaces scanner errors from advance" {
    var parser = try Parser.init("\"unterminated", testing.allocator);
    defer parser.deinit(testing.allocator);
    try testing.expectError(ParseError.UnterminatedString, parser.advance());
}

test "parser error helpers return the supplied error" {
    var parser = try Parser.init("x", testing.allocator);
    defer parser.deinit(testing.allocator);
    try parser.advance();
    try parser.advance();
    try testing.expectError(ParseError.ExpectEOF, parser.errorAtCurrent(ParseError.ExpectEOF));
    try testing.expectError(ParseError.VarMissingIdentifier, parser.errorAtPrevious(ParseError.VarMissingIdentifier));
}

test "parser tracks token line numbers" {
    var parser = try Parser.init("a\nb", testing.allocator);
    defer parser.deinit(testing.allocator);
    try parser.advance();
    try testing.expectEqual(@as(usize, 1), parser.current.line);
    try parser.advance();
    try testing.expectEqual(@as(usize, 2), parser.current.line);
}

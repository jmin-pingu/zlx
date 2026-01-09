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

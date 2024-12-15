const std = @import("std");
const token_type = @import("token_type.zig");

const TokenType = token_type.TokenType;

pub const Token = struct {
    ttype: token_type.TokenType,
    lexeme: []const u8, 
    literal: ?[]const u8, 
    line: u64, 

    pub fn new(ttype: token_type.TokenType, lexeme: []const u8, literal: ?[]const u8, line: u64) Token {
        return Token {
            .ttype = ttype,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn to_string(self: *Token, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{any} {s} {any}\n", .{self.ttype, self.lexeme, self.literal}) catch unreachable;
    }
};

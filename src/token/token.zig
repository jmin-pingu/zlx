const std = @import("std");
const token_type = @import("token_type.zig");

const TokenType = token_type.TokenType;
// TODO: incorporate Literal union 
// const literal = enum{identifier, string, number};
// const Literal = union{
//     identifier: []const u8,
//     string: []const u8,
//     number: f64,
// };

pub const Token = struct {
    ttype: token_type.TokenType,
    lexeme: []const u8, 
    literal: ?[]const u8, 
    line: u64, 

    pub fn new(ttype: token_type.TokenType, lexeme: []const u8, literal: ?[]const u8, line: u64) Token {
        // Depending on token_type, add a corresponding literal
        
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

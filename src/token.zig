const std = @import("std");
const token_type = @import("token_type.zig");

const TokenType = token_type.TokenType;

pub const Token = struct {
    ttype: token_type.TokenType,
    lexeme: []const u8, 
    literal: []const u8, 
    line: u64, 

    pub fn new(ttype: token_type.TokenType, lexeme: []const u8, literal: []const u8, line: u64) Token {

    }

    pub to_string(allocator: std.mem.Allocator) []const u8 {
        
    }
}

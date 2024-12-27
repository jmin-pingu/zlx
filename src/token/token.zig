const std = @import("std");
const token_type = @import("token_type.zig");

const TokenType = token_type.TokenType;

// union for literal values
const LiteralTag = enum{Identifier, String, Number, Nil, Bool};
pub const LiteralValue = union(LiteralTag){
    Identifier: []const u8,
    String: []const u8,
    Number: f64,
    Nil: ?void,
    Bool: bool,
};

pub const Token = struct {
    ttype: token_type.TokenType,
    lexeme: []const u8, 
    literal: LiteralValue, 
    line: u64, 

    pub fn new(ttype: token_type.TokenType, lexeme: []const u8, literal: ?[]const u8, line: u64) Token {
        // Depending on token_type, add a corresponding literal
        switch (ttype) {
            .IDENTIFIER => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    // TODO: double check optional unwrapping here
                    .literal = LiteralValue{.Identifier=literal.?},
                    .line = line,
                };
            }, 
            .STRING => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = LiteralValue{.String=literal.?},
                    .line = line,
                };
            }, 
            .NUMBER => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = LiteralValue{.Number=std.fmt.parseFloat(f64, literal.?) catch unreachable},
                    .line = line,
                };
            },
            .FALSE => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = LiteralValue{.Bool=false},
                    .line = line,
                };
            },
            .TRUE => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = LiteralValue{.Bool=true},
                    .line = line,
                };
            },
            else => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = LiteralValue{.Nil=null},
                    .line = line,
                };
            }
        }
    }

    pub fn to_string(self: *Token, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{any} {s} {any}\n", .{self.ttype, self.lexeme, self.literal}) catch unreachable;
    }
};

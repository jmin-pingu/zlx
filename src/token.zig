const std = @import("std");
const TokenType = @import("token_type.zig").TokenType;
const Literal = @import("literal.zig").Literal;
const Tag = @import("literal.zig").Tag;

// union for literal values
// Data Oriented Programming: we are fundamentally wasting space with literal since more often than not, it will be Nil
pub const Token = struct {
    ttype: TokenType,
    lexeme: []const u8, 
    literal: Literal, 
    line: u64, 

    pub fn new(ttype: TokenType, lexeme: []const u8, literal: ?[]const u8, line: u64) Token {
        // Depending on token_type, add a corresponding literal
        switch (ttype) {
            .IDENTIFIER => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    // TODO: double check optional unwrapping here
                    .literal = Literal{.Identifier=literal.?},
                    .line = line,
                };
            }, 
            .STRING => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = Literal{.String=literal.?},
                    .line = line,
                };
            }, 
            .NUMBER => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = Literal{.Number=std.fmt.parseFloat(f64, literal.?) catch unreachable},
                    .line = line,
                };
            },
            .FALSE => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = Literal{.Bool=false},
                    .line = line,
                };
            },
            .TRUE => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = Literal{.Bool=true},
                    .line = line,
                };
            },
            else => {
                return Token {
                    .ttype = ttype,
                    .lexeme = lexeme,
                    .literal = Literal{.Nil=null},
                    .line = line,
                };
            }
        }
    }

    pub fn to_string(self: *Token, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{any} {s} {any}\n", .{self.ttype, self.lexeme, self.literal}) catch unreachable;
    }
};


test "literal_test" {
    const x0 = Literal{ .Identifier = "bar" };
    // const x1 = Literal{ .String = "foo" };
    // const x2 = Literal{ .Number = 10 };
    // const x3 = Literal{ .Nil = null };
    // const x4 = Literal{ .Bool = true };
    std.debug.print("{}\n", .{x0.check_tag(Tag.Identifier)});
}

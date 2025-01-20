const std = @import("std");
const token_type = @import("token_type.zig");
const RuntimeError = @import("error.zig").RuntimeError;
 
const TokenType = token_type.TokenType;

pub const Tag = enum{Identifier, String, Number, Nil, Bool};

// TODO: Literal is a wrapper, much like Object in Java 
pub const Literal = union(Tag){
    Identifier: []const u8,
    String: []const u8,
    Number: f64,
    Nil: ?void,
    Bool: bool,

    // Private methods
    fn check_tag(self: Literal, tag: Tag) bool {
        return std.mem.eql(u8, @tagName(self), @tagName(tag));
    }

    fn same_tags(self: Literal, other: Literal, tag: Tag) bool {
        return self.check_tag(tag) and other.check_tag(tag);
    }

    fn is_truthy(self: Literal) bool {
        if (self.check_tag(Tag.Nil)) {
            return false;
        } else if (self.check_tag(Tag.Bool)) {
            return self.Bool;
        } else {
            return true;
        }
    }

    // TODO: I don't know how to to `Identifier`s so I have to consider that later on.
    fn equals(self: Literal, other: Literal) bool {
        if (self.same_tags(other, Tag.Nil)) {
            return true;
        } else if (self.check_tag(Tag.Nil) or self.check_tag(Tag.Nil)) {
            return false;
        } else if (self.same_tags(other, Tag.String)) {
            return std.mem.eql(u8, self.String, other.String);
        } else if (self.same_tags(other, Tag.Number) or self.check_tag(Tag.Bool) or other.check_tag(Tag.Bool)) {
            const a: f64 = if (self.check_tag(Tag.Bool)) @floatFromInt(@intFromBool(self.Bool)) else self.Number;
            const b: f64 = if (other.check_tag(Tag.Bool)) @floatFromInt(@intFromBool(other.Bool)) else other.Number;
            return a == b;
        } else {
            return false;
        }
    }

    // Public methods
    pub fn to_string(self: Literal, allocator: std.mem.Allocator) RuntimeError![]const u8 {
        switch (self) {
            .Number => |val| {
                return std.fmt.allocPrint(
                    allocator, 
                    "{d}", 
                    .{val}
                ) catch return RuntimeError.AllocError;
            },
            .Nil => {
                return "nil";
            },
            .Bool => |val| {
                return std.fmt.allocPrint(
                    allocator, 
                    "{any}", 
                    .{val}
                ) catch return RuntimeError.AllocError;
            },
            .Identifier => |val| {
                return val;
            },
            .String => |val| {
                return val;
            },
        }
    }

    pub fn evaluate_unary(self: Literal, ttype: TokenType) RuntimeError!Literal {
        switch (ttype) {
            TokenType.MINUS => {
                // TODO: check numeric
                if (!self.check_tag(Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = -self.Number};
            },
            TokenType.BANG => {
                return Literal{ .Bool = !self.is_truthy()};
            },
            else => {
                return RuntimeError.OperatorError;
            }
        }
    }

    pub fn evaluate_binary(self: Literal, other: Literal, ttype: TokenType, allocator: std.mem.Allocator) RuntimeError!Literal {
        switch (ttype) {
            TokenType.MINUS => {
                if (!self.same_tags(other, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = self.Number - other.Number};
            },
            TokenType.SLASH => {
                if (!self.same_tags(other, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = self.Number / other.Number};

            },
            TokenType.STAR => {
                if (!self.same_tags(other, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = self.Number * other.Number};
            },
            TokenType.PLUS => {
                // Check if both string OR both numeric
                if (!self.same_tags(other, Tag.String) and !self.same_tags(other, Tag.Number)) return RuntimeError.OperandError;
                if (self.check_tag(Tag.String)) {
                    const new_string = std.fmt.allocPrint(
                        allocator, 
                        "{s}{s}", 
                        .{self.String, other.String}
                    ) catch return RuntimeError.AllocError;
                    return Literal{ .String = new_string};

                } else {
                    return Literal{ .Number = self.Number + other.Number};
                }
            },
            TokenType.GREATER => {
                if (!self.same_tags(other, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Bool = self.Number > other.Number};
            },
            TokenType.GREATER_EQUAL => {
                if (!self.same_tags(other, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Bool = self.Number >= other.Number};
            },
            TokenType.LESS => {
                if (!self.same_tags(other, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Bool = self.Number < other.Number};
            },
            TokenType.LESS_EQUAL => {
                if (!self.same_tags(other, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Bool = self.Number <= other.Number};
            },
            TokenType.BANG_EQUAL => {
                return Literal{ .Bool = !self.equals(other)};
            },
            TokenType.EQUAL_EQUAL => {
                return Literal{ .Bool = self.equals(other)};
            },
            else => {
                return RuntimeError.OperatorError;
            }
        }
    }
};

test "to_string_test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    try std.testing.expectEqualStrings(try (Literal{ .Nil = null}).to_string(allocator), "nil");
    try std.testing.expectEqualStrings(try (Literal{ .Number = 10.10}).to_string(allocator), "10.1");
    try std.testing.expectEqualStrings(try (Literal{ .String = "foo"}).to_string(allocator), "foo");
    try std.testing.expectEqualStrings(try (Literal{ .Identifier = "var"}).to_string(allocator), "var");
    try std.testing.expectEqualStrings(try (Literal{ .Bool = true}).to_string(allocator), "true");
}

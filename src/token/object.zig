const std = @import("std");

const token_type = @import("token_type.zig");
const TokenType = token_type.TokenType;

const err = @import("../error.zig");
const AllocationError = err.AllocationError;
const Function = @import("../function.zig").Function;
 
// Literal should be defined similar to Object in Java
pub const Tag = enum{Identifier, String, Number, Nil, Bool, Function};
pub const Object = union(Tag){
    Identifier: []const u8,
    String: []const u8,
    Number: f64,
    Nil: ?void,
    Bool: bool,
    @"Function": *Function,

    pub fn checkTag(self: Object, tag: Tag) bool {
        return std.mem.eql(u8, @tagName(self), @tagName(tag));
    }

    pub fn sameTags(self: Object, other: Object, tag: Tag) bool {
        return self.checkTag(tag) and other.checkTag(tag);
    }

    pub fn isTruthy(self: Object) bool {
        if (self.checkTag(Tag.Nil)) {
            return false;
        } else if (self.checkTag(Tag.Bool)) {
            return self.Bool;
        } else {
            return true;
        }
    }

    pub fn equals(self: Object, other: Object) bool {
        if (self.sameTags(other, Tag.Nil)) {
            return true;
        } else if (self.checkTag(Tag.Nil) or self.checkTag(Tag.Nil)) {
            return false;
        } else if (self.sameTags(other, Tag.String)) {
            return std.mem.eql(u8, self.String, other.String);
        } else if (self.sameTags(other, Tag.Number) or self.checkTag(Tag.Bool) or other.checkTag(Tag.Bool)) {
            const a: f64 = if (self.checkTag(Tag.Bool)) @floatFromInt(@intFromBool(self.Bool)) else self.Number;
            const b: f64 = if (other.checkTag(Tag.Bool)) @floatFromInt(@intFromBool(other.Bool)) else other.Number;
            return a == b;
        } else {
            return false;
        }
    }

    // Public methods
    pub fn toString(self: Object, allocator: std.mem.Allocator) AllocationError![]const u8 {
        switch (self) {
            .Number => |val| {
                return std.fmt.allocPrint(
                    allocator, 
                    "{d}", 
                    .{val}
                ) catch return err.outOfMemory();
            },
            .Nil => {
                return "nil";
            },
            .Bool => |val| {
                return std.fmt.allocPrint(
                    allocator, 
                    "{any}", 
                    .{val}
                ) catch return err.outOfMemory();
            },
            .Identifier => |val| {
                return val;
            },
            .String => |val| {
                return val;
            },
            .Function => |function|{
                return try function.initCallable().toString(allocator);
            },
        }
    }
};

test "to_string_test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    try std.testing.expectEqualStrings(try (Object{ .Nil = null}).toString(allocator), "nil");
    try std.testing.expectEqualStrings(try (Object{ .Number = 10.10}).toString(allocator), "10.1");
    try std.testing.expectEqualStrings(try (Object{ .String = "foo"}).toString(allocator), "foo");
    try std.testing.expectEqualStrings(try (Object{ .Identifier = "var"}).toString(allocator), "var");
    try std.testing.expectEqualStrings(try (Object{ .Bool = true}).toString(allocator), "true");
}

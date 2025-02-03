const std = @import("std");
const ArrayList = std.ArrayList;
// import files into namespace
const zlx = @import("zlx.zig");
const Error = @import("error.zig").Error;

pub fn main() !void {
    try zlx.main();
}

test "test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

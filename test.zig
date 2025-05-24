const std = @import("std");

const Error = error{
    EqualityError,
};

pub fn main() void {
    const a: u8 =1;
    std.debug.print("outer: {d}\n", .{a});
}

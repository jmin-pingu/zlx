const std = @import("std");

const Error = error{
    EqualityError,

};


pub fn main() void {
    std.debug.print("{!}", .{Error.new(Error.EqualityError)});
}

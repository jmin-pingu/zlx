const std = @import("std");
 
pub const Error = error{
    UsageError,
    SyntaxError,
    OutOfMemory,
    JSONError,
    RunError,
    ReadError,
    WriteError,
    FileError,
    PrintError,
    GenericError,
    AllocError,
};

pub fn error_msg(line_number: usize, message: []const u8, allocator: std.mem.Allocator) Error {
    return report(line_number, "", message, allocator);
}

fn report(line_number: usize, where: []const u8, message: []const u8, allocator: std.mem.Allocator) Error {
    std.debug.print("{s}", .{std.fmt.allocPrint(allocator, "[line: {d}] Error{s}: {s}\n", .{line_number, where, message}) catch return Error.AllocError});
    return Error.SyntaxError;
}

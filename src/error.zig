const std = @import("std");
 
// TODO: improve error unions
pub const Error = error{
    UsageError,
    SyntaxError,
    OutOfMemory,
    ParseError,
    JSONError,
    RuntimeError,
    ReadError,
    WriteError,
    FileError,
    PrintError,
    GenericError,
    AllocError,
};

pub fn error_msg(line_number: usize, message: []const u8, err: Error, allocator: std.mem.Allocator) Error {
    return report(line_number, "", message, err, allocator);
}

fn report(line_number: usize, where: []const u8, message: []const u8, err: Error, allocator: std.mem.Allocator) Error {
    std.debug.print("{s}", .{std.fmt.allocPrint(allocator, "[line: {d}] Error{s}, {!}: {s}\n", .{line_number, where, err, message}) catch return Error.AllocError});
    return err;
}

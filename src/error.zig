const std = @import("std");
 
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

const SyntaxError = error {};

pub const RuntimeError = error {
    DivideByZero,
    OperandError,
    OperatorError,
    AllocError,
};


// Responsibility is on the programmer to handle errors
pub fn error_msg(line_number: usize, message: []const u8, err: Error, allocator: std.mem.Allocator) Error {
    std.debug.print("{s}", .{std.fmt.allocPrint(allocator, "[line: {d}] {!}: {s}\n", .{line_number, err, message}) catch return Error.AllocError});
    return err;
}


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
    AssignmentError,
    VariableShadow,
    FunctionCallError,
    TooManyArguments,
};

const SyntaxError = error {};

pub const RuntimeError = error {
    DivideByZero,
    OperandError,
    OperatorError,
    AllocError,
    UninitializedVariable,
    FunctionCallError,
    UndeclaredVariable,
    TooManyArguments,
};


// Responsibility is on the programmer to handle errors
pub fn error_msg(line_number: usize, message: []const u8, err: Error, allocator: std.mem.Allocator) Error {
    std.debug.print("{s}", .{std.fmt.allocPrint(allocator, "[line: {d}] {!}: {s}\n", .{line_number, err, message}) catch return Error.AllocError});
    return err;
}

/// runtime_error_msg
/// note, line_number is on the burden of the programmer to provider
pub fn runtime_error_msg(line_number: ?usize, message: []const u8, err: RuntimeError, allocator: std.mem.Allocator) RuntimeError {
    if (line_number) |num| {
        std.debug.print(
            "{s}", 
            .{
                std.fmt.allocPrint(
                    allocator, 
                    "[line: {d}] {!}: {s}\n", 
                    .{num, err, message}
                ) catch return RuntimeError.AllocError
            }
        );
    } else {
        std.debug.print(
            "{s}", 
            .{
                std.fmt.allocPrint(
                    allocator, 
                    "{!}: {s}\n", 
                    .{err, message}
                ) catch return RuntimeError.AllocError
            }
        );

    }

    return err;
}

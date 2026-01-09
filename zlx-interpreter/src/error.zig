const std = @import("std");
 
pub const AllocationError = error {
    OutOfMemory
};

pub const Error = error{
    UsageError,
} || AllocationError || FileError || RuntimeError || ParseError;

pub const FileError = error{
    StdoutError,
    OpenError,
    ReadError,
};

pub const RuntimeError = error{ OperatorError, OperandError } || AllocationError || FunctionError || EnvironmentError || VariableError || ClassError;

pub const CompileError = error{ 
    VariableShadow, 
    RepeatVariableDeclaration,
    RecursiveInheritanceError,
    IncorrectReturnScope,
    IncorrectBreakScope,
    IncorrectThisScope,
    IncorrectInitScope,
    IncorrectSuperScope,
} || AllocationError;

pub const FunctionError = error {
    TooManyArguments,
    FunctionCallError,
} || AllocationError;

pub const ClassError = error {
    InvalidPropertyAccess,
    InvalidFieldAccess,
    UndefinedProperty,
} || AllocationError;

pub const ParseError = error {
    EndOfFile,
    SyntaxError,
} || AllocationError || FunctionError || VariableError;

pub const ScannerError = error {
    InvalidCharacter,
    UnterminatedString,
} || AllocationError;

pub const VariableError = error {
    UninitializedVariable,
    UndeclaredVariable,
    AssignmentError, 
} || AllocationError;


pub const EnvironmentError = error {
    UndeclaredObject,
    UninitalizedObject,
} || AllocationError;

pub fn outOfMemory() AllocationError {
    std.debug.print("[panic] out of memory\n", .{});
    return AllocationError.OutOfMemory;
}

pub fn errorMessage(
    comptime ErrorType: type, 
    line_number: ?usize, 
    message: []const u8, 
    err: ErrorType, 
    allocator: std.mem.Allocator
) ErrorType {
    if (@typeInfo(ErrorType) != .error_set) @compileError("ErrorType must be an .ErrorSet");

    if (line_number) |num| {
        std.debug.print(
            "{s}", 
            .{
                std.fmt.allocPrint(allocator, "[line: {d}] {}: {s}\n", .{num, err, message}) catch return outOfMemory()
            }
        );
    } else {
        std.debug.print(
            "{s}", 
            .{
                std.fmt.allocPrint(allocator, "{}: {s}\n", .{err, message}) catch return outOfMemory()
            }
        );

    }
    return err;
}

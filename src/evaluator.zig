const std = @import("std");
const expr = @import("expr.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token_type.zig").TokenType;

const literal = @import("literal.zig");
const LiteralValue = @import("literal.zig").Literal;
const LiteralTag = @import("literal.zig").Tag;
const RuntimeError = @import("error.zig").RuntimeError;
const Error = @import("error.zig").Error;

pub fn interpret(e: *const expr.Expr, allocator: std.mem.Allocator) Error!void { 
    const value = evaluate(e, allocator) catch return Error.RuntimeError;
    std.debug.print("{s}\n", .{value.to_string(allocator) catch return Error.RuntimeError});
}

fn evaluate(ex: *const expr.Expr, allocator: std.mem.Allocator) Error!LiteralValue { 
    // TODO: all RuntimeError handling should be done here 
    switch (ex.dtype) {
        .Binary => |e| {
            // Now evaluate should return a RuntimeError
            const left = try evaluate(e.left, allocator);
            // error_msg(line_number: usize, message: []const u8, err: Error, allocator: std.mem.Allocator) Error {
            const right = try evaluate(e.right, allocator);
            return left.evaluate_binary(right, e.operator.ttype, allocator) 
                catch |runtime_err| {
                return runtime_error_msg(runtime_err, e.operator , allocator);
            };
        },
        .Grouping => |e| {
            return evaluate(e.expression, allocator);
        },
        .Literal => |e| {
            return e.value;
        },
        .Unary => |e| {
            const right = try evaluate(e.right, allocator);
            return right.evaluate_unary(e.operator.ttype) catch |runtime_err| {
                return runtime_error_msg(runtime_err, e.operator, allocator);
            };
        },
    }
}
 
fn runtime_error_msg(runtime_err: RuntimeError, token: Token, allocator: std.mem.Allocator) Error {
    const message = "";
    // DivideByZero,
    // OperandError,
    // OperatorError,
    // AllocError,
    std.debug.print("{s}", .{
        std.fmt.allocPrint(
            allocator, 
            "[line: {d}] RuntimeError: {!} {s} {s}\n", 
            .{token.line, runtime_err, message, token.lexeme}
        ) catch return Error.AllocError}
    );
    return Error.RuntimeError; 
}

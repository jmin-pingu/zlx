const std = @import("std");
const expr = @import("expr.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token_type.zig").TokenType;

const literal = @import("literal.zig");
const LiteralValue = @import("literal.zig").Literal;
const LiteralTag = @import("literal.zig").Tag;
const Error = @import("error.zig").Error;
const err = @import("error.zig").err;

pub fn interpret(e: *const expr.Expr, allocator: std.mem.Allocator) Error!LiteralValue { 
    const value = try evaluate(e, allocator) catch |Error.RuntimeError| {};
    std.debug.print("{s}\n", .{try value.to_string(allocator) catch unreachable});
}

fn evaluate(ex: *const expr.Expr, allocator: std.mem.Allocator) Error!LiteralValue { 
    // TODO: add error messaging for runtime
    switch (ex.dtype) {
        .Binary => |e| {
            const left = evaluate(e.left, allocator) catch return Error.RuntimeError;
            const right = evaluate(e.right, allocator) catch return Error.RuntimeError;
            return literal.evaluate_binary(left, right, e.operator);
        },
        .Grouping => |e| {
            return evaluate(e.expression);
        },
        .Literal => |e| {
            return e;
        },
        .Unary => |e| {
            const right = evaluate(e.right, allocator) catch return Error.RuntimeError;
            return literal.evaluate_unary(right, e.operator);
        },
    }
}


const std = @import("std");
const expr = @import("expr.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token_type.zig").TokenType;

const literal = @import("literal.zig");
const LiteralValue = @import("literal.zig").Literal;
const LiteralTag = @import("literal.zig").Tag;
const Error = @import("error.zig").Error;
const err = @import("error.zig").err;

pub fn interpret(e: *const expr.Expr, allocator: std.mem.Allocator) Error!void { 
    const value = evaluate(e, allocator) catch return Error.RuntimeError;
    std.debug.print("{s}\n", .{try value.to_string(allocator)});
}

fn evaluate(ex: *const expr.Expr, allocator: std.mem.Allocator) Error!LiteralValue { 
    // TODO: add error messaging for runtime
    switch (ex.dtype) {
        .Binary => |e| {
            const left = evaluate(e.left, allocator) catch return Error.RuntimeError;
            const right = evaluate(e.right, allocator) catch return Error.RuntimeError;
            return left.evaluate_binary(right, e.operator.ttype, allocator);
        },
        .Grouping => |e| {
            return evaluate(e.expression, allocator);
        },
        .Literal => |e| {
            return e.value;
        },
        .Unary => |e| {
            const right = evaluate(e.right, allocator) catch return Error.RuntimeError;
            return right.evaluate_unary(e.operator.ttype);
        },
    }
}


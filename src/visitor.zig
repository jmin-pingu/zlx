const std = @import("std");
const expr = @import("expr.zig");
const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const Error = @import("error.zig").Error;

pub fn visit(e: *const expr.Expr, allocator: std.mem.Allocator) Error![]const u8 { 

    switch (e.dtype) {
        .Binary => |binary_expr| {
            var expressions = allocator.alloc(*const expr.Expr, 2) catch return Error.AllocError;
            expressions[0] = binary_expr.left;
            expressions[1] = binary_expr.right;
            return try parenthesize(binary_expr.operator.lexeme, expressions, allocator);
        },
        .Grouping => |grouping_expr| {
            var expressions = allocator.alloc(*const expr.Expr, 1)  catch return Error.AllocError;
            expressions[0] = grouping_expr.expression;
            return try parenthesize("group", expressions, allocator);
        },
        .Literal => |literal_expr| {
            // NOTE: literal is the base case
            if (literal_expr.value) |val| {
                return val; 
            } else {
                return Error.ParseError;
            }
        },
        .Unary => |unary_expr| {
            var expressions = allocator.alloc(*const expr.Expr, 1) catch return Error.AllocError;
            expressions[0] = unary_expr.right;
            return try parenthesize(unary_expr.operator.lexeme, expressions, allocator);
        },
    }
}

fn parenthesize(name: []const u8, expressions: []*const expr.Expr, allocator: std.mem.Allocator) Error![]const u8 {
    var interior: []const u8 = "";
    for (expressions) |expression| {
        const parenthesized = visit(expression, allocator) catch |err| {return err;};
        interior = std.fmt.allocPrint(
            allocator, 
            "{s} {s}", 
            .{interior, parenthesized}
        ) catch return Error.AllocError;
    }
    return std.fmt.allocPrint(allocator, "({s}{s} )", .{name, interior}) catch return Error.AllocError;
}

test "instantiation_test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const expression = expr.Binary.new(
        expr.Literal.new("10", allocator),
        Token.new(TokenType.STAR, "*", null, 1),
        expr.Literal.new("11", allocator),
        allocator
    );
    const out = visit(expression, allocator) catch unreachable;
    std.debug.print("{s}\n", .{out});
}

test "visit_test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const expression = expr.Binary.new(
        expr.Unary.new(
            Token.new(TokenType.MINUS, "-", null, 1),
            expr.Literal.new("123", allocator),
            allocator
        ),
        Token.new(TokenType.STAR, "*", null, 1),
        expr.Grouping.new(
            expr.Literal.new("45.67", allocator),
            allocator
        ),
        allocator
    );
    const out = visit(expression, allocator) catch unreachable;
    std.debug.print("{s}\n", .{out});
}

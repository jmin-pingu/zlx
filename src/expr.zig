pub const std = @import("std");
const Token = @import("token/token.zig").Token;
const LiteralValue = @import("token/token.zig").LiteralValue;
const TokenType = @import("token/token_type.zig").TokenType;
pub const ExprTypeEnum = enum {Binary, Grouping, Literal, Unary};
pub const ExprType = union(ExprTypeEnum) {Binary: Binary, Grouping: Grouping, Literal: Literal, Unary: Unary};

pub const Expr = struct {
    dtype: ExprType,
    pub fn new(dtype: ExprType) Expr {
        return Expr{ .dtype = dtype };
    }
};

pub const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
    pub fn new(left: *const Expr, operator: Token, right: *const Expr, allocator: std.mem.Allocator) *const Expr {
        const new_expr = allocator.create(Expr) catch unreachable;
        new_expr.* = Expr.new(ExprType{ .Binary=Binary{ .left=left, .operator=operator, .right=right, }});
        return new_expr;
    }
};

pub const Grouping = struct {
    expression: *const Expr,
    pub fn new(expression: *const Expr, allocator: std.mem.Allocator) *const Expr {
        const new_expr = allocator.create(Expr) catch unreachable;
        new_expr.* = Expr.new(ExprType{ .Grouping=Grouping{ .expression=expression, }});
        return new_expr;
    }
};

pub const Literal = struct {
    // Probably change a line here
    value: LiteralValue,
    pub fn new(value: LiteralValue, allocator: std.mem.Allocator) *const Expr {
        const new_expr = allocator.create(Expr) catch unreachable;
        new_expr.* = Expr.new(ExprType{ .Literal=Literal{ .value=value, }});
        return new_expr;
    }
};

pub const Unary = struct {
    operator: Token,
    right: *const Expr,
    pub fn new(operator: Token, right: *const Expr, allocator: std.mem.Allocator) *const Expr {
        const new_expr = allocator.create(Expr) catch unreachable;
        new_expr.* = Expr.new(ExprType{ .Unary=Unary{ .operator=operator, .right=right, }});
        return new_expr;
    }
};


pub const std = @import("std");
const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const ExprTypeEnum = enum {Binary, Grouping, Literal, Unary};
const ExprType = union(ExprTypeEnum) {binary: Binary, grouping: Grouping, literal: Literal, unary: Unary};

pub const Expr = struct {
    dtype: ExprType,
    pub fn new(dtype: ExprType) Expr {
        return Expr{ .dtype = dtype };
    }
};

pub const Binary = struct {
    left: Expr,
    operator: Token,
    right: Expr,
    pub fn new(left: Expr, operator: Token, right: Expr) Binary {
        return Binary{ .left=left, .operator=operator, .right=right, };
    }
};

pub const Grouping = struct {
    expression: Expr,
    pub fn new(expression: Expr) Grouping {
        return Grouping{ .expression=expression, };
    }
};

pub const Literal = struct {
    value: ?[]const u8,
    pub fn new(value: ?[]const u8) Literal {
        return Literal{ .value=value, };
    }
};

pub const Unary = struct {
    operator: Token,
    right: Expr,
    pub fn new(operator: Token, right: Expr) Unary {
        return Unary{ .operator=operator, .right=right, };
    }
};


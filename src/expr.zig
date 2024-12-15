pub const std = @import("std");
const Token = @import("token/token.zig").Token;
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
    pub fn new(left: *const Expr, operator: Token, right: *const Expr) Binary {
        return Binary{ .left=left, .operator=operator, .right=right, };
    }
};

pub const Grouping = struct {
    expression: *const Expr,
    pub fn new(expression: *const Expr) Grouping {
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
    right: *const Expr,
    pub fn new(operator: Token, right: *const Expr) Unary {
        return Unary{ .operator=operator, .right=right, };
    }
};


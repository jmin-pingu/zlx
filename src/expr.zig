pub const std = @import("std");
const Token = @import("token.zig").Token;
const LiteralValue = @import("literal.zig").Literal;
const Error = @import("error.zig").Error;
const Visitor = @import("visitor.zig").Visitor;

// Then for each Binary, Grouping, Literal, and Unary, the implementation is the same!
pub const ExprTag = enum {Binary, Grouping, Literal, Unary};
pub const Expr= union(ExprTag) {
    Binary: Binary, 
    Grouping: Grouping, 
    Literal: Literal, 
    Unary: Unary,

    pub fn accept(self: *const Expr, T: type, visitor: Visitor(T)) T {
        switch (self.*) {
            inline else => |*case| return case.accept(T, visitor),
        }
    }
};
 
pub const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
    pub fn new(
        left: *const Expr, 
        operator: Token, 
        right: *const Expr, 
        allocator: std.mem.Allocator
    ) *const Expr {
        const expr = allocator.create(Expr) catch unreachable;
        expr.* = Expr { 
            .Binary= Binary{ 
                .left=left, .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: *const Binary, T: type, visitor: Visitor(T)) T { 
        return try visitor.visitBinaryExpr(self);
    }
};

pub const Grouping = struct {
    expression: *const Expr,
    pub fn new(
        expression: *const Expr, 
        allocator: std.mem.Allocator
    ) *const Expr {
        const expr = allocator.create(Expr) catch unreachable;
        expr.* = Expr{ 
            .Grouping= Grouping{ 
                .expression=expression, 
            }
        };
        return expr;
    }

    pub fn accept(self: *const Grouping, T: type, visitor: Visitor(T)) T { 
        return try visitor.visitGroupingExpr(self);
    }
};

pub const Literal = struct {
    // NOTE: I really don't like having LiteralValue in token.zig 
    value: LiteralValue,
    pub fn new(value: LiteralValue, allocator: std.mem.Allocator) *const Expr {
        const expr = allocator.create(Expr) catch unreachable;
        expr.* = Expr{ 
            .Literal=Literal{ 
                .value=value, 
            }
        };
        return expr;
    }

    pub fn accept(self: *const Literal, T: type, visitor: Visitor(T)) T { 
        return try visitor.visitLiteralExpr(self);
    }
};

pub const Unary = struct {
    operator: Token,
    right: *const Expr,
    pub fn new(operator: Token, right: *const Expr, allocator: std.mem.Allocator) *const Expr {
        const expr = allocator.create(Expr) catch unreachable;
        expr.* = Expr{ 
            .Unary=Unary{ 
                .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: *const Unary, T: type, visitor: Visitor(T)) T { 
        return try visitor.visitUnaryExpr(self);
    }
};


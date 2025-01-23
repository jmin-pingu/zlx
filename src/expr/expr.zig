const std = @import("std");
const Visitor = @import("visitor.zig").ExprVisitor;

// Custom Imports
const Error = @import("../error.zig").Error;
const Token = @import("../token/token.zig").Token;
const LiteralValue = @import("../token/literal.zig").Literal;


pub const Expr= union(enum) {
    binary: Binary, 
    grouping: Grouping, 
    literal: Literal, 
    unary: Unary,
    @"var": Var,
    assign: Assign,

    pub fn accept(self: *const Expr, T: type, visitor: Visitor(T)) T {
        switch (self.*) {
            inline else => |*case| return case.accept(T, visitor),
        }
    }
};
 
// NOTE: does it make for us to operate on `*const Expr` instead of `Expr`
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
            .binary= Binary{ 
                .left=left, .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: *const Binary, T: type, visitor: Visitor(T)) T { 
        return visitor.visitBinaryExpr(self);
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
            .grouping= Grouping{ 
                .expression=expression, 
            }
        };
        return expr;
    }

    pub fn accept(self: *const Grouping, T: type, visitor: Visitor(T)) T { 
        return visitor.visitGroupingExpr(self);
    }
};

pub const Literal = struct {
    // NOTE: naming, I really don't like having LiteralValue in token.zig 
    value: LiteralValue,
    pub fn new(value: LiteralValue, allocator: std.mem.Allocator) *const Expr {
        const expr = allocator.create(Expr) catch unreachable;
        expr.* = Expr{ 
            .literal=Literal{ 
                .value=value, 
            }
        };
        return expr;
    }

    pub fn accept(self: *const Literal, T: type, visitor: Visitor(T)) T { 
        return visitor.visitLiteralExpr(self);
    }
};

pub const Unary = struct {
    operator: Token,
    right: *const Expr,
    pub fn new(operator: Token, right: *const Expr, allocator: std.mem.Allocator) *const Expr {
        const expr = allocator.create(Expr) catch unreachable;
        expr.* = Expr{ 
            .unary=Unary{ 
                .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: *const Unary, T: type, visitor: Visitor(T)) T { 
        return visitor.visitUnaryExpr(self);
    }
};

pub const Var = struct {
    name: Token,
    pub fn new(name: Token, allocator: std.mem.Allocator) *const Expr {
        const expr = allocator.create(Expr) catch unreachable;
        expr.* = Expr{ 
            .@"var"=Var{ 
                .name=name,
            }
        };
        return expr;
    }

    pub fn accept(self: *const Var, T: type, visitor: Visitor(T)) T { 
        return visitor.visitVarExpr(self);
    }
};

pub const Assign = struct {
    name: Token,
    value: *const Expr,
    pub fn new(name: Token, value: *const Expr, allocator: std.mem.Allocator) *const Expr {
        const expr = allocator.create(Expr) catch unreachable;
        expr.* = Expr{ 
            .assign=Assign{ 
                .name=name,
                .value=value,
            }
        };
        return expr;
    }

    pub fn accept(self: *const Assign, T: type, visitor: Visitor(T)) T { 
        return visitor.visitAssignExpr(self);
    }
};

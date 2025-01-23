const std = @import("std");
const Visitor = @import("visitor.zig").StmtVisitor;

// Custom Imports
const Token = @import("../token/token.zig").Token;
const e = @import("../expr/expr.zig");
const ArrayList = std.ArrayList;

pub const Stmt= union(enum) {
    expression: Expression, 
    block: Block, 
    print: Print, 
    @"var": Var, 

    pub fn accept(self: *const Stmt, T: type, visitor: Visitor(T)) T {
        switch (self.*) {
            inline else => |*case| return case.accept(T, visitor),
        }
    }
};
 
pub const Block = struct { 
    statements: ArrayList(Stmt),

    pub fn accept(self: Block, T: type, visitor: Visitor(T)) T {
        return visitor.visitBlockStmt(self);
    }

    pub fn new(statements: ArrayList(Stmt)) Stmt {
        return Stmt{ .block=Block{.statements=statements} };
    }
};

pub const Expression = struct { 
    expression: *const e.Expr,

    pub fn accept(self: Expression, T: type, visitor: Visitor(T)) T {
        return visitor.visitExpressionStmt(self);
    }

    pub fn new(expr: *const e.Expr) Stmt {
        return Stmt{ .expression=Expression{.expression=expr} };
    }
};

pub const Print = struct { 
    expression: *const e.Expr,

    pub fn accept(self: Print, T: type, visitor: Visitor(T)) T {
        return visitor.visitPrintStmt(self);
    }

    pub fn new(expr: *const e.Expr) Stmt {
        return Stmt{ .print=Print{.expression=expr} };
    }
};

pub const Var = struct { 
    name: Token,
    initializer: ?*const e.Expr, 

    pub fn accept(self: Var, T: type, visitor: Visitor(T)) T {
        return visitor.visitVarStmt(self);
    }

    pub fn new(name: Token, initializer: ?*const e.Expr) Stmt {
        return Stmt{ .@"var"=Var{.name=name, .initializer=initializer} };
    }
};


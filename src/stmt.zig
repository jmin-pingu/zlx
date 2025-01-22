const std = @import("std");
const e = @import("expr.zig");
const Token = @import("token.zig").Token;
const StmtVisitor = @import("visitor.zig").StmtVisitor;
 
pub const Stmt= union(enum) {
    expression: Expression, 
    print: Print, 
    @"var": Var, 

    pub fn accept(self: *const Stmt, T: type, visitor: StmtVisitor(T)) T {
        switch (self.*) {
            inline else => |*case| return case.accept(T, visitor),
        }
    }
};
 
pub const Expression = struct { 
    expression: *const e.Expr,

    pub fn accept(self: Expression, T: type, visitor: StmtVisitor(T)) T {
        return visitor.visitExpressionStmt(self);
    }

    pub fn new(expr: *const e.Expr) Stmt {
        return Stmt{ .expression=Expression{.expression=expr} };
    }
};

pub const Print = struct { 
    expression: *const e.Expr,

    pub fn accept(self: Print, T: type, visitor: StmtVisitor(T)) T {
        return visitor.visitPrintStmt(self);
    }

    pub fn new(expr: *const e.Expr) Stmt {
        return Stmt{ .print=Print{.expression=expr} };
    }
};

pub const Var = struct { 
    name: Token,
    initializer: ?*const e.Expr, // NOTE: not a null pointer, this is either null OR a pointer

    pub fn accept(self: Var, T: type, visitor: StmtVisitor(T)) T {
        return visitor.visitVarStmt(self);
    }

    pub fn new(name: Token, initializer: ?*const e.Expr) Stmt {
        return Stmt{ .@"var"=Var{.name=name, .initializer=initializer} };
    }
};


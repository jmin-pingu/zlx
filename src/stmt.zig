const std = @import("std");

const Error = @import("error.zig").Error;
// Type Imports
const Expr= @import("expr.zig").Expr;
const Token = @import("token/token.zig").Token;
const e = @import("expr.zig");
const ArrayList = std.ArrayList;

pub const Stmt= union(enum) {
    expression: Expression, 
    block: Block, 
    print: Print, 
    @"var": Var, 
    @"if": If,

    pub fn accept(self: *const Stmt, T: type, visitor: Visitor(T)) T {
        switch (self.*) {
            inline else => |*case| return case.accept(T, visitor),
        }
    }
};
 
pub const If = struct { 
    condition: *const Expr, 
    then_branch: *const Stmt,
    else_branch: ?*const Stmt,

    pub fn accept(self: If, T: type, visitor: Visitor(T)) T {
        return visitor.visitIfStmt(self);
    }

    pub fn new(condition: *const Expr, then_branch: Stmt, else_branch: ?Stmt, allocator: std.mem.Allocator) Error!Stmt {
        const then_b = allocator.create(Stmt) catch return Error.AllocError;

        then_b.* = then_branch;
        if (else_branch) |value| {
            const else_b = allocator.create(Stmt) catch return Error.AllocError;
            else_b.* = value;
            return Stmt{ 
                .@"if"= If{
                    .condition=condition,
                    .then_branch=then_b,
                    .else_branch=else_b,
                } 
            };
        } else {
            return Stmt{ 
                .@"if"= If{
                    .condition=condition,
                    .then_branch=then_b,
                    .else_branch=null,
                } 
            };
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

/// Visitor interface implementation
pub fn Visitor(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *anyopaque,
        // Below define all the visit functions
        // TODO: structure with Vtable formatting later
        visitExpressionStmtFn: *const fn (*anyopaque, stmt: Expression) T,
        visitPrintStmtFn: *const fn (*anyopaque, stmt: Print) T,
        visitVarStmtFn: *const fn (*anyopaque, stmt: Var) T,
        visitBlockStmtFn: *const fn (*anyopaque, stmt: Block) T,
        visitIfStmtFn: *const fn (*anyopaque, stmt: If) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
            if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn visitExpressionStmtImpl(pointer: *anyopaque, stmt: Expression) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitExpressionStmt, .{self, stmt});
                }

                pub fn visitPrintStmtImpl(pointer: *anyopaque, stmt: Print) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitPrintStmt, .{self, stmt});
                }

                pub fn visitVarStmtImpl(pointer: *anyopaque, stmt: Var) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitVarStmt, .{self, stmt});
                }

                pub fn visitBlockStmtImpl(pointer: *anyopaque, stmt: Block) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitBlockStmt, .{self, stmt});
                }

                pub fn visitIfStmtImpl(pointer: *anyopaque, stmt: If) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitIfStmt, .{self, stmt});
                }
            };
        
            return .{
                .ptr = ptr,
                .visitExpressionStmtFn = gen.visitExpressionStmtImpl,
                .visitPrintStmtFn = gen.visitPrintStmtImpl,
                .visitVarStmtFn = gen.visitVarStmtImpl,
                .visitBlockStmtFn = gen.visitBlockStmtImpl,
                .visitIfStmtFn = gen.visitIfStmtImpl,
            };
        }

        pub inline fn visitExpressionStmt(self: Self, stmt: Expression) T {
            return self.visitExpressionStmtFn(self.ptr, stmt);
        }

        pub inline fn visitPrintStmt(self: Self, stmt: Print) T {
            return self.visitPrintStmtFn(self.ptr, stmt);
        }

        pub inline fn visitVarStmt(self: Self, stmt: Var) T {
            return self.visitVarStmtFn(self.ptr, stmt);
        }

        pub inline fn visitBlockStmt(self: Self, stmt: Block) T {
            return self.visitBlockStmtFn(self.ptr, stmt);
        }

        pub inline fn visitIfStmt(self: Self, stmt: If) T {
            return self.visitIfStmtFn(self.ptr, stmt);
        }
    };
}


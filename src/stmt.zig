const std = @import("std");

const err = @import("error.zig");
const Error = err.Error;
const AllocationError = err.AllocationError;
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
    @"while": While,
    @"break": Break,
    function: Function,
    @"return": Return,
    // @"return": Return,

    pub fn accept(self: *const Stmt, T: type, visitor: Visitor(T)) T {
        switch (self.*) {
            inline else => |case| return case.accept(T, visitor),
        }
    }
};

pub const Return = struct { 
    keyword: Token,
    value: ?*Expr,

    pub fn accept(self: Return, T: type, visitor: Visitor(T)) T {
        return visitor.visitReturnStmt(self);
    }

    pub fn new(keyword: Token, value: ?*Expr) Stmt {
        return Stmt { 
            .@"return"= Return {
                .keyword= keyword, 
                .value = value
            } 
        };
    }
};

 
pub const Function = struct { 
    name: Token,
    params: ArrayList(Token),
    body: ArrayList(Stmt),

    pub fn accept(self: Function, T: type, visitor: Visitor(T)) T {
        return visitor.visitFunctionStmt(self);
    }

    pub fn new(name: Token, params: ArrayList(Token), body: ArrayList(Stmt)) AllocationError!Stmt {
        return Stmt{ 
            .function= Function {
                .name= name, 
                .params = params.clone() catch return err.outOfMemory(),
                .body= body.clone() catch return err.outOfMemory() 
            } 
        };
    }
};


pub const Break = struct { 
    associated_condition: *Expr,

    pub fn accept(self: Break, T: type, visitor: Visitor(T)) T {
        return visitor.visitBreakStmt(self);
    }

    pub fn new(associated_condition: *Expr) Stmt {
        return Stmt{ .@"break"=Break{.associated_condition =associated_condition} };
    }
};


pub const While = struct { 
    condition: *Expr,
    body: *Stmt,

    pub fn accept(self: While, T: type, visitor: Visitor(T)) T {
        return visitor.visitWhileStmt(self);
    }

    pub fn new(condition: *Expr, body: Stmt, allocator: std.mem.Allocator) AllocationError!Stmt {
        const new_body = allocator.create(Stmt) catch return err.outOfMemory();
        new_body.* = body;
        return Stmt{ .@"while"=While{.condition=condition, .body = new_body} };
    }
};

pub const If = struct { 
    condition: *Expr, 
    then_branch: *Stmt,
    else_branch: ?*Stmt,

    pub fn accept(self: If, T: type, visitor: Visitor(T)) T {
        return visitor.visitIfStmt(self);
    }

    pub fn new(condition: *Expr, then_branch: Stmt, else_branch: ?Stmt, allocator: std.mem.Allocator) AllocationError!Stmt {
        const then_b = allocator.create(Stmt) catch return err.outOfMemory();

        then_b.* = then_branch;
        if (else_branch) |value| {
            const else_b = allocator.create(Stmt) catch return err.outOfMemory();
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
    expression: *e.Expr,

    pub fn accept(self: Expression, T: type, visitor: Visitor(T)) T {
        return visitor.visitExpressionStmt(self);
    }

    pub fn new(expr: *e.Expr) Stmt {
        return Stmt{ .expression=Expression{.expression=expr} };
    }
};

pub const Print = struct { 
    expression: *e.Expr,

    pub fn accept(self: Print, T: type, visitor: Visitor(T)) T {
        return visitor.visitPrintStmt(self);
    }

    pub fn new(expr: *e.Expr) Stmt {
        return Stmt{ .print=Print{.expression=expr} };
    }
};

pub const Var = struct { 
    name: Token,
    initializer: ?*e.Expr, 

    pub fn accept(self: Var, T: type, visitor: Visitor(T)) T {
        return visitor.visitVarStmt(self);
    }

    pub fn new(name: Token, initializer: ?*e.Expr) Stmt {
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
        visitWhileStmtFn: *const fn (*anyopaque, stmt: While) T,
        visitBreakStmtFn: *const fn (*anyopaque, stmt: Break) T,
        visitFunctionStmtFn: *const fn (*anyopaque, stmt: Function) T,
        visitReturnStmtFn: *const fn (*anyopaque, stmt: Return) T,

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

                pub fn visitWhileStmtImpl(pointer: *anyopaque, stmt: While) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitWhileStmt, .{self, stmt});
                }

                pub fn visitBreakStmtImpl(pointer: *anyopaque, stmt: Break) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitBreakStmt, .{self, stmt});
                }

                pub fn visitFunctionStmtImpl(pointer: *anyopaque, stmt: Function) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitFunctionStmt, .{self, stmt});
                }

                pub fn visitReturnStmtImpl(pointer: *anyopaque, stmt: Return) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitReturnStmt, .{self, stmt});
                }
            };
        
            return .{
                .ptr = ptr,
                .visitExpressionStmtFn = gen.visitExpressionStmtImpl,
                .visitPrintStmtFn = gen.visitPrintStmtImpl,
                .visitVarStmtFn = gen.visitVarStmtImpl,
                .visitBlockStmtFn = gen.visitBlockStmtImpl,
                .visitIfStmtFn = gen.visitIfStmtImpl,
                .visitWhileStmtFn = gen.visitWhileStmtImpl,
                .visitBreakStmtFn = gen.visitBreakStmtImpl,
                .visitFunctionStmtFn = gen.visitFunctionStmtImpl,
                .visitReturnStmtFn = gen.visitReturnStmtImpl,
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

        pub inline fn visitWhileStmt(self: Self, stmt: While) T {
            return self.visitWhileStmtFn(self.ptr, stmt);
        }

        pub inline fn visitBreakStmt(self: Self, stmt: Break) T {
            return self.visitBreakStmtFn(self.ptr, stmt);
        }

        pub inline fn visitFunctionStmt(self: Self, stmt: Function) T {
            return self.visitFunctionStmtFn(self.ptr, stmt);
        }

        pub inline fn visitReturnStmt(self: Self, stmt: Return) T {
            return self.visitReturnStmtFn(self.ptr, stmt);
        }
    };
}


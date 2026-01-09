const std = @import("std");
// Type Imports
const e = @import("expr.zig");
const Expr= e.Expr;
const Token = @import("token.zig").Token;
const err = @import("../error.zig");
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
    class: Class,

    pub fn accept(self: *Stmt, T: type, visitor: Visitor(T)) T {
        switch (self.*) {
            inline else => |case| return case.accept(T, visitor),
        }
    }

    pub fn activeField(self: *Stmt) void {
        switch (self.*) {
            inline else => |s| std.debug.print("  |---{}\n", .{@TypeOf(s)}),
        }
    }
};

pub const Class = struct { 
    name: Token,
    superclass: ?*Expr,
    methods: ArrayList(*Stmt),

    pub fn accept(self: Class, T: type, visitor: Visitor(T)) T {
        return visitor.visitClassStmt(self);
    }

    pub fn new(name: Token, superclass: ?*Expr, methods: ArrayList(*Stmt), allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt { 
            .class= Class {
                .name = name, 
                .superclass = superclass, 
                .methods = methods
            } 
        };
        return stmt_ref;
    }
};

pub const Return = struct { 
    keyword: Token,
    value: ?*Expr,

    pub fn accept(self: Return, T: type, visitor: Visitor(T)) T {
        return visitor.visitReturnStmt(self);
    }

    pub fn new(keyword: Token, value: ?*Expr, allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt { 
            .@"return"= Return {
                .keyword= keyword, 
                .value = value
            } 
        };
        return stmt_ref;
    }
};

 
pub const Function = struct { 
    name: Token,
    params: ArrayList(Token),
    body: ArrayList(*Stmt),

    pub fn accept(self: Function, T: type, visitor: Visitor(T)) T {
        return visitor.visitFunctionStmt(self);
    }

    pub fn new(name: Token, params: ArrayList(Token), body: ArrayList(*Stmt), allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt{ 
            .function= Function {
                .name= name, 
                .params = params.clone(allocator) catch return err.outOfMemory(),
                .body= body.clone(allocator) catch return err.outOfMemory() 
            } 
        };
        return stmt_ref;
    }
};


pub const Break = struct { 
    keyword: Token,
    associated_condition: *Expr,

    pub fn accept(self: Break, T: type, visitor: Visitor(T)) T {
        return visitor.visitBreakStmt(self);
    }

    pub fn new(keyword: Token, associated_condition: *Expr, allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt{ 
            .@"break"=Break{
                .keyword=keyword,
                .associated_condition=associated_condition
            } 
        };
        return stmt_ref;
    }
};


pub const While = struct { 
    condition: *Expr,
    body: *Stmt,

    pub fn accept(self: While, T: type, visitor: Visitor(T)) T {
        return visitor.visitWhileStmt(self);
    }

    pub fn new(condition: *Expr, body: *Stmt, allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt{ .@"while"=While{.condition=condition, .body = body} };
        return stmt_ref;
    }
};

pub const If = struct { 
    condition: *Expr, 
    then_branch: *Stmt,
    else_branch: ?*Stmt,

    pub fn accept(self: If, T: type, visitor: Visitor(T)) T {
        return visitor.visitIfStmt(self);
    }

    pub fn new(condition: *Expr, then_branch: *Stmt, else_branch: ?*Stmt, allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        // const then_b = allocator.create(Stmt) catch return err.outOfMemory();

        if (else_branch) |value| {
            stmt_ref.* = Stmt{ 
                .@"if"= If{
                    .condition=condition,
                    .then_branch=then_branch,
                    .else_branch=value,
                } 
            };
        } else {
            stmt_ref.* = Stmt{ 
                .@"if"= If{
                    .condition=condition,
                    .then_branch=then_branch,
                    .else_branch=null,
                } 
            };

        }
        return stmt_ref;

    }
};

pub const Block = struct { 
    statements: ArrayList(*Stmt),

    pub fn accept(self: Block, T: type, visitor: Visitor(T)) T {
        return visitor.visitBlockStmt(self);
    }

    pub fn new(statements: ArrayList(*Stmt), allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt{ .block=Block{.statements=statements} };
        return stmt_ref;
    }
};

pub const Expression = struct { 
    expression: *e.Expr,

    pub fn accept(self: Expression, T: type, visitor: Visitor(T)) T {
        return visitor.visitExpressionStmt(self);
    }

    pub fn new(expr: *e.Expr, allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt{ .expression=Expression{.expression=expr} };
        return stmt_ref;
    }
};

pub const Print = struct { 
    expression: *e.Expr,

    pub fn accept(self: Print, T: type, visitor: Visitor(T)) T {
        return visitor.visitPrintStmt(self);
    }

    pub fn new(expr: *e.Expr, allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt{ .print=Print{.expression=expr} };
        return stmt_ref;
    }
};

pub const Var = struct { 
    name: Token,
    initializer: ?*e.Expr, 

    pub fn accept(self: Var, T: type, visitor: Visitor(T)) T {
        return visitor.visitVarStmt(self);
    }

    pub fn new(name: Token, initializer: ?*e.Expr, allocator: std.mem.Allocator) err.AllocationError!*Stmt {
        const stmt_ref = allocator.create(Stmt) catch return err.outOfMemory();
        stmt_ref.* = Stmt{ .@"var"=Var{.name=name, .initializer=initializer} };
        return stmt_ref;
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
        visitClassStmtFn: *const fn (*anyopaque, stmt: Class) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .pointer) @compileError("ptr must be a pointer");
            if (ptr_info.pointer.size != .one) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn visitExpressionStmtImpl(pointer: *anyopaque, stmt: Expression) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitExpressionStmt, .{self, stmt});
                }

                pub fn visitPrintStmtImpl(pointer: *anyopaque, stmt: Print) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitPrintStmt, .{self, stmt});
                }

                pub fn visitVarStmtImpl(pointer: *anyopaque, stmt: Var) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitVarStmt, .{self, stmt});
                }

                pub fn visitBlockStmtImpl(pointer: *anyopaque, stmt: Block) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitBlockStmt, .{self, stmt});
                }

                pub fn visitIfStmtImpl(pointer: *anyopaque, stmt: If) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitIfStmt, .{self, stmt});
                }

                pub fn visitWhileStmtImpl(pointer: *anyopaque, stmt: While) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitWhileStmt, .{self, stmt});
                }

                pub fn visitBreakStmtImpl(pointer: *anyopaque, stmt: Break) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitBreakStmt, .{self, stmt});
                }

                pub fn visitFunctionStmtImpl(pointer: *anyopaque, stmt: Function) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitFunctionStmt, .{self, stmt});
                }

                pub fn visitReturnStmtImpl(pointer: *anyopaque, stmt: Return) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitReturnStmt, .{self, stmt});
                }

                pub fn visitClassStmtImpl(pointer: *anyopaque, stmt: Class) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitClassStmt, .{self, stmt});
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
                .visitClassStmtFn = gen.visitClassStmtImpl,
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

        pub inline fn visitClassStmt(self: Self, stmt: Class) T {
            return self.visitClassStmtFn(self.ptr, stmt);
        }
    };
}


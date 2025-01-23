const std = @import("std");
const e= @import("expr.zig");
const Expr= @import("expr.zig").Expr;

const s= @import("stmt.zig");
const Error = @import("error.zig").Error;
// References:
// - https://zig.news/kilianvounckx/zig-interfaces-for-the-uninitiated-an-update-4gf1
// - https://github.com/ziglang/zig/blob/master/lib/std/Random.zig
// - https://github.com/ziglang/zig/blob/master/lib/std/mem/Allocator.zig
// - https://revivalizer.xyz/post/the-missing-zig-polymorphism-reference/

/// ExprVisitor interface implementation
pub fn ExprVisitor(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *anyopaque,
        // Below define all the visit functions
        // TODO: structure with Vtable formatting later
        visitBinaryExprFn: *const fn (*anyopaque, expr: *const e.Binary) T,
        visitGroupingExprFn: *const fn (*anyopaque, expr: *const e.Grouping) T,
        visitLiteralExprFn: *const fn (*anyopaque, expr: *const e.Literal) T,
        visitUnaryExprFn: *const fn (*anyopaque, expr: *const e.Unary) T,
        visitVarExprFn: *const fn (*anyopaque, expr: *const e.Var) T,
        visitAssignExprFn: *const fn (*anyopaque, expr: *const e.Assign) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
            if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn visitBinaryExprImpl(pointer: *anyopaque, expr: *const e.Binary) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitBinaryExpr, .{self, expr});
                }

                pub fn visitGroupingExprImpl(pointer: *anyopaque, expr: *const e.Grouping) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitGroupingExpr, .{self, expr});
                }

                pub fn visitLiteralExprImpl(pointer: *anyopaque, expr: *const e.Literal) T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitLiteralExpr, .{self, expr});
                }

                pub fn visitUnaryExprImpl(pointer: *anyopaque, expr: *const e.Unary) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitUnaryExpr, .{self, expr});
                }

                pub fn visitVarExprImpl(pointer: *anyopaque, expr: *const e.Var) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitVarExpr, .{self, expr});
                }

                pub fn visitAssignExprImpl(pointer: *anyopaque, expr: *const e.Assign) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitAssignExpr, .{self, expr});
                }
            };
        
            return .{
                .ptr = ptr,
                .visitBinaryExprFn = gen.visitBinaryExprImpl,
                .visitGroupingExprFn = gen.visitGroupingExprImpl,
                .visitLiteralExprFn = gen.visitLiteralExprImpl,
                .visitUnaryExprFn = gen.visitUnaryExprImpl,
                .visitVarExprFn = gen.visitVarExprImpl,
                .visitAssignExprFn = gen.visitAssignExprImpl,
            };
        }

        pub inline fn visitBinaryExpr(self: Self, expr: *const e.Binary) T {
            return self.visitBinaryExprFn(self.ptr, expr);
        }

        pub inline fn visitGroupingExpr(self: Self, expr: *const e.Grouping) T {
            return self.visitGroupingExprFn(self.ptr, expr);
        }

        pub inline fn visitLiteralExpr(self: Self, expr: *const e.Literal) T {
            return self.visitLiteralExprFn(self.ptr, expr);
        }

        pub inline fn visitUnaryExpr(self: Self, expr: *const e.Unary) T {
            return self.visitUnaryExprFn(self.ptr, expr);
        }

        pub inline fn visitVarExpr(self: Self, expr: *const e.Var) T {
            return self.visitVarExprFn(self.ptr, expr);
        }

        pub inline fn visitAssignExpr(self: Self, expr: *const e.Assign) T {
            return self.visitAssignExprFn(self.ptr, expr);
        }
    };
}

/// StmtVisitor interface implementation
pub fn StmtVisitor(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *anyopaque,
        // Below define all the visit functions
        // TODO: structure with Vtable formatting later
        visitExpressionStmtFn: *const fn (*anyopaque, stmt: s.Expression) T,
        visitPrintStmtFn: *const fn (*anyopaque, stmt: s.Print) T,
        visitVarStmtFn: *const fn (*anyopaque, stmt: s.Var) T,
        visitBlockStmtFn: *const fn (*anyopaque, stmt: s.Block) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
            if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn visitExpressionStmtImpl(pointer: *anyopaque, stmt: s.Expression) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitExpressionStmt, .{self, stmt});
                }

                pub fn visitPrintStmtImpl(pointer: *anyopaque, stmt: s.Print) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitPrintStmt, .{self, stmt});
                }

                pub fn visitVarStmtImpl(pointer: *anyopaque, stmt: s.Var) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitVarStmt, .{self, stmt});
                }

                pub fn visitBlockStmtImpl(pointer: *anyopaque, stmt: s.Block) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitBlockStmt, .{self, stmt});
                }
            };
        
            return .{
                .ptr = ptr,
                .visitExpressionStmtFn = gen.visitExpressionStmtImpl,
                .visitPrintStmtFn = gen.visitPrintStmtImpl,
                .visitVarStmtFn = gen.visitVarStmtImpl,
                .visitBlockStmtFn = gen.visitBlockStmtImpl,
            };
        }

        pub inline fn visitExpressionStmt(self: Self, stmt: s.Expression) T {
            return self.visitExpressionStmtFn(self.ptr, stmt);
        }

        pub inline fn visitPrintStmt(self: Self, stmt: s.Print) T {
            return self.visitPrintStmtFn(self.ptr, stmt);
        }

        pub inline fn visitVarStmt(self: Self, stmt: s.Var) T {
            return self.visitVarStmtFn(self.ptr, stmt);
        }

        pub inline fn visitBlockStmt(self: Self, stmt: s.Block) T {
            return self.visitBlockStmtFn(self.ptr, stmt);
        }
    };
}


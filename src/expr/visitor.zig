const std = @import("std");
const Expr= @import("expr.zig").Expr;
// Types
const Binary= @import("expr.zig").Binary;
const Grouping= @import("expr.zig").Grouping;
const Literal= @import("expr.zig").Literal;
const Unary= @import("expr.zig").Unary;
const Var= @import("expr.zig").Var;
const Assign= @import("expr.zig").Assign;
     
// Other Imports
const Error = @import("../error.zig").Error;

/// ExprVisitor interface implementation
pub fn ExprVisitor(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *anyopaque,
        // Below define all the visit functions
        visitBinaryExprFn: *const fn (*anyopaque, expr: *const Binary) T,
        visitGroupingExprFn: *const fn (*anyopaque, expr: *const Grouping) T,
        visitLiteralExprFn: *const fn (*anyopaque, expr: *const Literal) T,
        visitUnaryExprFn: *const fn (*anyopaque, expr: *const Unary) T,
        visitVarExprFn: *const fn (*anyopaque, expr: *const Var) T,
        visitAssignExprFn: *const fn (*anyopaque, expr: *const Assign) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
            if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn visitBinaryExprImpl(pointer: *anyopaque, expr: *const Binary) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitBinaryExpr, .{self, expr});
                }

                pub fn visitGroupingExprImpl(pointer: *anyopaque, expr: *const Grouping) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitGroupingExpr, .{self, expr});
                }

                pub fn visitLiteralExprImpl(pointer: *anyopaque, expr: *const Literal) T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitLiteralExpr, .{self, expr});
                }

                pub fn visitUnaryExprImpl(pointer: *anyopaque, expr: *const Unary) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitUnaryExpr, .{self, expr});
                }

                pub fn visitVarExprImpl(pointer: *anyopaque, expr: *const Var) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitVarExpr, .{self, expr});
                }

                pub fn visitAssignExprImpl(pointer: *anyopaque, expr: *const Assign) T {
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

        pub inline fn visitBinaryExpr(self: Self, expr: *const Binary) T {
            return self.visitBinaryExprFn(self.ptr, expr);
        }

        pub inline fn visitGroupingExpr(self: Self, expr: *const Grouping) T {
            return self.visitGroupingExprFn(self.ptr, expr);
        }

        pub inline fn visitLiteralExpr(self: Self, expr: *const Literal) T {
            return self.visitLiteralExprFn(self.ptr, expr);
        }

        pub inline fn visitUnaryExpr(self: Self, expr: *const Unary) T {
            return self.visitUnaryExprFn(self.ptr, expr);
        }

        pub inline fn visitVarExpr(self: Self, expr: *const Var) T {
            return self.visitVarExprFn(self.ptr, expr);
        }

        pub inline fn visitAssignExpr(self: Self, expr: *const Assign) T {
            return self.visitAssignExprFn(self.ptr, expr);
        }
    };
}


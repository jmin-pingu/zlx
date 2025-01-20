const std = @import("std");
const e= @import("expr.zig");
const Expr= @import("expr.zig").Expr;
const Error = @import("error.zig").Error;
// Visitor interface implementation
// References:
// - https://zig.news/kilianvounckx/zig-interfaces-for-the-uninitiated-an-update-4gf1
// - https://github.com/ziglang/zig/blob/master/lib/std/Random.zig
// - https://github.com/ziglang/zig/blob/master/lib/std/mem/Allocator.zig
// - https://revivalizer.xyz/post/the-missing-zig-polymorphism-reference/


pub fn Visitor(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *anyopaque,
        // Below define all the visit functions
        visitBinaryExprFn: *const fn (*anyopaque, expr: *const e.Binary) T,
        visitGroupingExprFn: *const fn (*anyopaque, expr: *const e.Grouping) T,
        visitLiteralExprFn: *const fn (*anyopaque, expr: *const e.Literal) T,
        visitUnaryExprFn: *const fn (*anyopaque, expr: *const e.Unary) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
        
            // Check if pointer is of correct type
            if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
            if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
            // Get pointer alignment since anyopaque does not have alignment
            // const alignment = ptr_info.Pointer.alignment;
        
            const gen = struct {
                pub fn visitBinaryExprImpl(pointer: *anyopaque, expr: *const e.Binary) T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitBinaryExpr, .{self, expr});
                }

                pub fn visitGroupingExprImpl(pointer: *anyopaque, expr: *const e.Grouping) T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitGroupingExpr, .{self, expr});
                }

                pub fn visitLiteralExprImpl(pointer: *anyopaque, expr: *const e.Literal) T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitLiteralExpr, .{self, expr});
                }

                pub fn visitUnaryExprImpl(pointer: *anyopaque, expr: *const e.Unary) T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitUnaryExpr, .{self, expr});
                }
            };
        
            return .{
                .ptr = ptr,
                .visitBinaryExprFn = gen.visitBinaryExprImpl,
                .visitGroupingExprFn = gen.visitGroupingExprImpl,
                .visitLiteralExprFn = gen.visitLiteralExprImpl,
                .visitUnaryExprFn = gen.visitUnaryExprImpl,
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
    };
}


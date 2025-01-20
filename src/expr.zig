pub const std = @import("std");
const Token = @import("token.zig").Token;
const LiteralValue = @import("literal.zig").Literal;
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
        visitBinaryExprFn: *const fn (*anyopaque, expr: *const Binary) Error!T,
        visitGroupingExprFn: *const fn (*anyopaque, expr: *const Grouping) Error!T,
        visitLiteralExprFn: *const fn (*anyopaque, expr: *const Literal) Error!T,
        visitUnaryExprFn: *const fn (*anyopaque, expr: *const Unary) Error!T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
        
            // Check if pointer is of correct type
            if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
            if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
            // Get pointer alignment since anyopaque does not have alignment
            // const alignment = ptr_info.Pointer.alignment;
        
            const gen = struct {
                pub fn visitBinaryExprImpl(pointer: *anyopaque, expr: *const Binary) Error!T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitBinaryExpr, .{self, expr});
                }

                pub fn visitGroupingExprImpl(pointer: *anyopaque, expr: *const Grouping) Error!T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitGroupingExpr, .{self, expr});
                }

                pub fn visitLiteralExprImpl(pointer: *anyopaque, expr: *const Literal) Error!T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitLiteralExpr, .{self, expr});
                }

                pub fn visitUnaryExprImpl(pointer: *anyopaque, expr: *const Unary) Error!T {
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

        pub inline fn visitBinaryExpr(self: Self, expr: *const Binary) Error!T {
            return self.visitBinaryExprFn(self.ptr, expr);
        }

        pub inline fn visitGroupingExpr(self: Self, expr: *const Grouping) Error!T {
            return self.visitGroupingExprFn(self.ptr, expr);
        }

        pub inline fn visitLiteralExpr(self: Self, expr: *const Literal) Error!T {
            return self.visitLiteralExprFn(self.ptr, expr);
        }

        pub inline fn visitUnaryExpr(self: Self, expr: *const Unary) Error!T {
            return self.visitUnaryExprFn(self.ptr, expr);
        }
    };
}

// Then for each Binary, Grouping, Literal, and Unary, the implementation is the same!
pub const ExprTag = enum {Binary, Grouping, Literal, Unary};
pub const Expr= union(ExprTag) {
    Binary: Binary, 
    Grouping: Grouping, 
    Literal: Literal, 
    Unary: Unary,

    pub fn accept(self: *const Expr, T: type, visitor: Visitor(T)) Error!T {
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

    pub fn accept(self: *const Binary, T: type, visitor: Visitor(T)) Error!T { 
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

    pub fn accept(self: *const Grouping, T: type, visitor: Visitor(T)) Error!T { 
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

    pub fn accept(self: *const Literal, T: type, visitor: Visitor(T)) Error!T { 
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

    pub fn accept(self: *const Unary, T: type, visitor: Visitor(T)) Error!T { 
        return try visitor.visitUnaryExpr(self);
    }
};


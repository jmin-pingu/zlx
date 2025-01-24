const std = @import("std");

// Custom Imports
const Error = @import("error.zig").Error;
const Token = @import("token/token.zig").Token;
const LiteralValue = @import("token/literal.zig").Literal;

pub const Expr= union(enum) {
    binary: Binary, 
    grouping: Grouping, 
    literal: Literal, 
    unary: Unary,
    @"var": Var,
    assign: Assign,
    logical: Logical,

    pub fn accept(self: *const Expr, T: type, visitor: Visitor(T)) T {
        switch (self.*) {
            inline else => |case| return case.accept(T, visitor),
        }
    }
};
 
// NOTE: does it make for us to operate on `*const Expr` instead of `Expr`
//
pub const Logical = struct {
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
            .logical= Logical{ 
                .left=left, .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: Logical, T: type, visitor: Visitor(T)) T { 
        return visitor.visitLogicalExpr(self);
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
            .binary= Binary{ 
                .left=left, .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: Binary, T: type, visitor: Visitor(T)) T { 
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

    pub fn accept(self: Grouping, T: type, visitor: Visitor(T)) T { 
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

    pub fn accept(self: Literal, T: type, visitor: Visitor(T)) T { 
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

    pub fn accept(self: Unary, T: type, visitor: Visitor(T)) T { 
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

    pub fn accept(self: Var, T: type, visitor: Visitor(T)) T { 
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

    pub fn accept(self: Assign, T: type, visitor: Visitor(T)) T { 
        return visitor.visitAssignExpr(self);
    }
};

/// Visitor interface implementation
pub fn Visitor(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *anyopaque,
        // NOTE: these a morbid pointers, use vtable
        visitBinaryExprFn: *const fn (*anyopaque, expr: Binary) T,
        visitGroupingExprFn: *const fn (*anyopaque, expr: Grouping) T,
        visitLiteralExprFn: *const fn (*anyopaque, expr: Literal) T,
        visitUnaryExprFn: *const fn (*anyopaque, expr: Unary) T,
        visitVarExprFn: *const fn (*anyopaque, expr: Var) T,
        visitAssignExprFn: *const fn (*anyopaque, expr: Assign) T,
        visitLogicalExprFn: *const fn (*anyopaque, expr: Logical) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
            if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn visitBinaryExprImpl(pointer: *anyopaque, expr: Binary) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitBinaryExpr, .{self, expr});
                }

                pub fn visitGroupingExprImpl(pointer: *anyopaque, expr: Grouping) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitGroupingExpr, .{self, expr});
                }

                pub fn visitLiteralExprImpl(pointer: *anyopaque, expr: Literal) T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.Pointer.child.visitLiteralExpr, .{self, expr});
                }

                pub fn visitUnaryExprImpl(pointer: *anyopaque, expr: Unary) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitUnaryExpr, .{self, expr});
                }

                pub fn visitVarExprImpl(pointer: *anyopaque, expr: Var) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitVarExpr, .{self, expr});
                }

                pub fn visitAssignExprImpl(pointer: *anyopaque, expr: Assign) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitAssignExpr, .{self, expr});
                }

                pub fn visitLogicalExprImpl(pointer: *anyopaque, expr: Logical) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitLogicalExpr, .{self, expr});
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
                .visitLogicalExprFn = gen.visitLogicalExprImpl,
            };
        }

        pub inline fn visitBinaryExpr(self: Self, expr: Binary) T {
            return self.visitBinaryExprFn(self.ptr, expr);
        }

        pub inline fn visitGroupingExpr(self: Self, expr: Grouping) T {
            return self.visitGroupingExprFn(self.ptr, expr);
        }

        pub inline fn visitLiteralExpr(self: Self, expr: Literal) T {
            return self.visitLiteralExprFn(self.ptr, expr);
        }

        pub inline fn visitUnaryExpr(self: Self, expr: Unary) T {
            return self.visitUnaryExprFn(self.ptr, expr);
        }

        pub inline fn visitVarExpr(self: Self, expr: Var) T {
            return self.visitVarExprFn(self.ptr, expr);
        }

        pub inline fn visitAssignExpr(self: Self, expr: Assign) T {
            return self.visitAssignExprFn(self.ptr, expr);
        }

        pub inline fn visitLogicalExpr(self: Self, expr: Logical) T {
            return self.visitLogicalExprFn(self.ptr, expr);
        }
    };
}


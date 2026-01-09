const std = @import("std");

// Custom Imports
const stmt = @import("stmt.zig");
const Token = @import("token.zig").Token;
const Object = @import("object.zig").Object;
const err = @import("../error.zig");
const ArrayList = std.ArrayList;

pub const Expr= union(enum) {
    binary: Binary, 
    grouping: Grouping, 
    call: Call, 
    get: Get,
    set: Set,
    this: This,
    super: Super,
    literal: Literal, 
    unary: Unary,
    @"var": Var,
    assign: Assign,
    logical: Logical,
    anonymous: Anonymous,

    pub fn accept(self: *Expr, T: type, visitor: Visitor(T)) T {
        switch (self.*) {
            inline else => |case| return case.accept(T, visitor, @intFromPtr(self)),
        }
    }

    pub fn activeField(self: *Expr) void {
        switch (self.*) {
            inline else => |s| std.debug.print("  |---{}\n", .{@TypeOf(s)}),
        }
    }
};

pub const Super = struct {
    keyword: Token,
    method: Token,
        
    pub fn new(keyword: Token, method: Token, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .super=Super{ 
                .keyword=keyword,
                .method=method,
            }
        };
        return expr;
    }

    pub fn accept(self: Super, T: type, visitor: Visitor(T), addr: usize) T { 
        return visitor.visitSuperExpr(self, addr);
    }
};


pub const This = struct {
    keyword: Token,
        
    pub fn new(keyword: Token, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .this=This{ 
                .keyword=keyword
            }
        };
        return expr;
    }

    pub fn accept(self: This, T: type, visitor: Visitor(T), addr: usize) T { 
        return visitor.visitThisExpr(self, addr);
    }
};


pub const Get = struct {
    object: *Expr,
    name: Token,
        
    pub fn new(name: Token, object: *Expr, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .get=Get{ 
                .name=name, .object=object 
            }
        };
        return expr;
    }

    pub fn accept(self: Get, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitGetExpr(self);
    }
};

pub const Set = struct {
    object: *Expr,
    name: Token,
    value: *Expr,
        
    pub fn new(name: Token, object: *Expr, value: *Expr, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .set=Set{ 
                .name=name, .object=object, .value = value
            }
        };
        return expr;
    }

    pub fn accept(self: Set, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitSetExpr(self);
    }
};



pub const Anonymous = struct {
    keyword: Token,
    function: stmt.Function,
        
    pub fn new(keyword: Token, function: stmt.Function, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .anonymous=Anonymous{ 
                .keyword=keyword, .function=function,
            }
        };
        return expr;
    }

    pub fn accept(self: Anonymous, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitAnonymousExpr(self);
    }
};

pub const Call = struct {
    callee: *Expr,
    paren: Token,
    arguments: ArrayList(*Expr),
        
    pub fn new(callee: *Expr, paren: Token, arguments: ArrayList(*Expr), allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .call=Call{ 
                .callee=callee, .paren=paren, .arguments=arguments,
            }
        };
        return expr;
    }

    pub fn accept(self: Call, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitCallExpr(self);
    }
};

pub const Logical = struct {
    left: *Expr,
    operator: Token,
    right: *Expr,

    pub fn new(
        left: *Expr, 
        operator: Token, 
        right: *Expr, 
        allocator: std.mem.Allocator
    ) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr { 
            .logical= Logical{ 
                .left=left, .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: Logical, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitLogicalExpr(self);
    }
};

pub const Binary = struct {
    left: *Expr,
    operator: Token,
    right: *Expr,
    pub fn new(
        left: *Expr, 
        operator: Token, 
        right: *Expr, 
        allocator: std.mem.Allocator
    ) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr { 
            .binary= Binary{ 
                .left=left, .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: Binary, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitBinaryExpr(self);
    }
};

pub const Grouping = struct {
    expression: *Expr,
    pub fn new(
        expression: *Expr, 
        allocator: std.mem.Allocator
    ) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return  err.outOfMemory();
        expr.* = Expr{ 
            .grouping= Grouping{ 
                .expression=expression, 
            }
        };
        return expr;
    }

    pub fn accept(self: Grouping, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitGroupingExpr(self);
    }
};

pub const Literal = struct {
    // NOTE: naming, I really don't like having LiteralValue in token.zig 
    value: Object,
    pub fn new(value: Object, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .literal=Literal{ 
                .value=value, 
            }
        };
        return expr;
    }

    pub fn accept(self: Literal, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitLiteralExpr(self);
    }
};

pub const Unary = struct {
    operator: Token,
    right: *Expr,
    pub fn new(operator: Token, right: *Expr, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .unary=Unary{ 
                .operator=operator, .right=right, 
            }
        };
        return expr;
    }

    pub fn accept(self: Unary, T: type, visitor: Visitor(T), addr: usize) T { 
        _ = addr;
        return visitor.visitUnaryExpr(self);
    }
};

pub const Var = struct {
    name: Token,
    pub fn new(name: Token, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        // std.debug.print("createVarExpr: name {s}, addr {d}\n", .{name.lexeme, @intFromPtr(expr)});
        expr.* = Expr{ 
            .@"var"=Var{ 
                .name=name,
            }
        };
        return expr;
    }

    pub fn accept(self: Var, T: type, visitor: Visitor(T), addr: usize) T { 
        return visitor.visitVarExpr(self, addr);
    }
};

pub const Assign = struct {
    name: Token,
    value: *Expr,
    pub fn new(name: Token, value: *Expr, allocator: std.mem.Allocator) err.AllocationError!*Expr {
        const expr = allocator.create(Expr) catch return err.outOfMemory();
        expr.* = Expr{ 
            .assign=Assign{ 
                .name=name,
                .value=value,
            }
        };

        // std.debug.print("createAssignExpr: name {s}, addr {d}\n", .{name.lexeme, @intFromPtr(expr)});
        return expr;
    }

    pub fn accept(self: Assign, T: type, visitor: Visitor(T), addr: usize) T { 
        return visitor.visitAssignExpr(self, addr);
    }
};

/// Visitor interface implementation
pub fn Visitor(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *anyopaque,
        // NOTE: these are morbid pointers, use vtable
        visitBinaryExprFn: *const fn (*anyopaque, expr: Binary) T,
        visitGroupingExprFn: *const fn (*anyopaque, expr: Grouping) T,
        visitLiteralExprFn: *const fn (*anyopaque, expr: Literal) T,
        visitUnaryExprFn: *const fn (*anyopaque, expr: Unary) T,
        visitVarExprFn: *const fn (*anyopaque, expr: Var, addr: usize) T,
        visitAssignExprFn: *const fn (*anyopaque, expr: Assign, addr: usize) T,
        visitLogicalExprFn: *const fn (*anyopaque, expr: Logical) T,
        visitCallExprFn: *const fn (*anyopaque, expr: Call) T,
        visitAnonymousExprFn: *const fn (*anyopaque, expr: Anonymous) T,
        visitGetExprFn: *const fn (*anyopaque, expr: Get) T,
        visitSetExprFn: *const fn (*anyopaque, expr: Set) T,
        visitThisExprFn: *const fn (*anyopaque, expr: This, addr: usize) T,
        visitSuperExprFn: *const fn (*anyopaque, expr: Super, addr: usize) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .pointer) @compileError("ptr must be a pointer");
            if (ptr_info.pointer.size != .one) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn visitBinaryExprImpl(pointer: *anyopaque, expr: Binary) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitBinaryExpr, .{self, expr});
                }

                pub fn visitGroupingExprImpl(pointer: *anyopaque, expr: Grouping) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitGroupingExpr, .{self, expr});
                }

                pub fn visitLiteralExprImpl(pointer: *anyopaque, expr: Literal) T {
                    // Cast pointer to correct alignment and type
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    // Call underlying function
                    return @call(.auto, ptr_info.pointer.child.visitLiteralExpr, .{self, expr});
                }

                pub fn visitUnaryExprImpl(pointer: *anyopaque, expr: Unary) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitUnaryExpr, .{self, expr});
                }

                pub fn visitVarExprImpl(pointer: *anyopaque, expr: Var, addr: usize) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitVarExpr, .{self, expr, addr});
                }

                pub fn visitAssignExprImpl(pointer: *anyopaque, expr: Assign, addr: usize) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitAssignExpr, .{self, expr, addr});
                }

                pub fn visitLogicalExprImpl(pointer: *anyopaque, expr: Logical) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitLogicalExpr, .{self, expr});
                }

                pub fn visitCallExprImpl(pointer: *anyopaque, expr: Call) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitCallExpr, .{self, expr});
                }

                pub fn visitAnonymousExprImpl(pointer: *anyopaque, expr: Anonymous) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitAnonymousExpr, .{self, expr});
                }

                pub fn visitGetExprImpl(pointer: *anyopaque, expr: Get) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitGetExpr, .{self, expr});
                }

                pub fn visitSetExprImpl(pointer: *anyopaque, expr: Set) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitSetExpr, .{self, expr});
                }

                pub fn visitThisExprImpl(pointer: *anyopaque, expr: This, addr: usize) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitThisExpr, .{self, expr, addr});
                }

                pub fn visitSuperExprImpl(pointer: *anyopaque, expr: Super, addr: usize) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.visitSuperExpr, .{self, expr, addr});
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
                .visitCallExprFn = gen.visitCallExprImpl,
                .visitAnonymousExprFn = gen.visitAnonymousExprImpl,
                .visitGetExprFn = gen.visitGetExprImpl,
                .visitSetExprFn = gen.visitSetExprImpl,
                .visitThisExprFn = gen.visitThisExprImpl,
                .visitSuperExprFn = gen.visitSuperExprImpl,
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

        pub inline fn visitVarExpr(self: Self, expr: Var, addr: usize) T {
            return self.visitVarExprFn(self.ptr, expr, addr);
        }

        pub inline fn visitAssignExpr(self: Self, expr: Assign, addr: usize) T {
            return self.visitAssignExprFn(self.ptr, expr, addr);
        }

        pub inline fn visitLogicalExpr(self: Self, expr: Logical) T {
            return self.visitLogicalExprFn(self.ptr, expr);
        }

        pub inline fn visitCallExpr(self: Self, expr: Call) T {
            return self.visitCallExprFn(self.ptr, expr);
        }

        pub inline fn visitAnonymousExpr(self: Self, expr: Anonymous) T {
            return self.visitAnonymousExprFn(self.ptr, expr);
        }

        pub inline fn visitGetExpr(self: Self, expr: Get) T {
            return self.visitGetExprFn(self.ptr, expr);
        }

        pub inline fn visitSetExpr(self: Self, expr: Set) T {
            return self.visitSetExprFn(self.ptr, expr);
        }

        pub inline fn visitThisExpr(self: Self, expr: This, addr: usize) T {
            return self.visitThisExprFn(self.ptr, expr, addr);
        }

        pub inline fn visitSuperExpr(self: Self, expr: Super, addr: usize) T {
            return self.visitSuperExprFn(self.ptr, expr, addr);
        }
    };
}


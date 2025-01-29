const std = @import("std");
const stmt = @import("stmt.zig");
const Callable = @import("callable.zig").Callable;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("environment.zig").Environment;
const Expr = @import("expr.zig").Expr;
const Object = @import("token/object.zig").Object;
const Error = @import("error.zig").Error;

const ArrayList = std.ArrayList;

pub const Function = struct {
    const Self = @This();
    declaration: ?stmt.Function, // NOTE: use a type union instead of stmt.Function, Native
    allocator: std.mem.Allocator,

    pub fn init(declaration: ?stmt.Function, allocator: std.mem.Allocator) Error!Function {
        return Function{.declaration = declaration, .allocator = allocator};
    }

    pub fn initCallable(self: *Self) Callable() {
        return Callable().init(self);
    }
    
    pub fn call(self: *Self, interpreter: *Interpreter, arguments: ArrayList(Object)) Error!Object {
        var environment = try Environment.init(self.allocator, interpreter.globals);
        for (0..self.declaration.?.params.items.len) |i| {
            std.debug.print("{s}:{any}\n", .{self.declaration.?.params.items[i].literal.Identifier, arguments.items[i]});
            try environment.define(self.declaration.?.params.items[i].literal.Identifier, arguments.items[i], self.allocator);
        }
        std.debug.print("body: {any}\n", .{self.declaration.?.body.items[0].print.expression.binary.right.@"var".name});
        interpreter.executeBlock(self.declaration.?.body, environment) catch return Error.FunctionCallError;
        return Object{.Nil = null};
    }

    pub fn arity(self: *Self) usize {
        return self.declaration.?.params.items.len;
    }

    pub fn toString(self: *Self) Error![]const u8 {
        return std.fmt.allocPrint(
            self.allocator,
            "<fn {s}>",
            .{self.declaration.?.name.lexeme}
        );
    }
};

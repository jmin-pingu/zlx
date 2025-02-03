const std = @import("std");
const stmt = @import("stmt.zig");
const Callable = @import("callable.zig").Callable;
const CallableType = @import("callable.zig").CallableType;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("environment.zig").Environment;
const Expr = @import("expr.zig").Expr;
const Object = @import("token/object.zig").Object;
const err = @import("error.zig");
const AllocationError = err.AllocationError;
const FunctionError = err.FunctionError;

const ArrayList = std.ArrayList;

pub const Function = struct {
    const Self = @This();
    callableType: CallableType, 

    pub fn init(declaration: stmt.Function) Function {
        return Function{.callableType = CallableType { .Declared = declaration}};
    }

    pub fn initCallable(self: *Self) Callable() {
        return Callable().init(self);
    }
    
    pub fn call(self: *Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) FunctionError!Object {
        // TODO: Depending on CallableType engage in different behavior
        var environment = Environment.init(allocator, interpreter.globals);
        // TODO: double-check logic of unwrap (.?)
        for (0..self.callableType.Declared.params.items.len) |i| {
            // std.debug.print("{s}:{any}\n", .{self.callableType.Declared.params.items[i].literal.Identifier, arguments.items[i]});
            try environment.define(self.callableType.Declared.params.items[i].literal.Identifier, arguments.items[i], allocator); // should return a variable error
        }
        interpreter.executeBlock(self.callableType.Declared.body, environment) catch return FunctionError.FunctionBodyError;
        return Object{.Nil = null};
    }

    pub fn arity(self: *Self) usize {
        return self.callableType.Declared.params.items.len;
    }

    pub fn toString(self: *Self, allocator: std.mem.Allocator) AllocationError![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "<fn {s}>",
            .{self.callableType.Declared.name.lexeme}
        ) catch return err.outOfMemory();
    }
};

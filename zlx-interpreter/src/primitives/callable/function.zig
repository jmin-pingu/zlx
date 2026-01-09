const std = @import("std");

const stmt = @import("../stmt.zig");
const Object = @import("../object.zig").Object;

const err = @import("../../error.zig");
const Interpreter = @import("../../interpreter.zig").Interpreter;
const Environment = @import("../../environment.zig").Environment;

const ArrayList = std.ArrayList;

pub const Function = struct {
    const Self = @This();
    declared: stmt.Function, 
    closure: *Environment,
    isInitializer: bool, 

    pub fn init(declared: stmt.Function, closure: *Environment, is_initializer: bool) err.AllocationError!Function {
        return Function{
            .declared = declared,
            .closure = closure,
            .isInitializer = is_initializer
        };
    }
    
    pub fn call(self: Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) err.FunctionError!Object {
        var environment = try allocator.create(Environment);
        environment.* = Environment.init(allocator, self.closure);

        for (0..self.declared.params.items.len) |i| {
            try environment.define(self.declared.params.items[i].literal.Identifier, arguments.items[i], allocator); // should return a variable error
        }

        const stmtValue = interpreter.executeBlock(self.declared.body, environment) catch return err.FunctionError.FunctionCallError;

        if (self.isInitializer) {
            return self.closure.getAt(0, "this", allocator) catch err.FunctionError.FunctionCallError;
        }

        if (stmtValue != null) {
            return stmtValue.?;
        }                         
        return Object{.Nil=null};
    }

    pub fn arity(self: Self) usize {
        return self.declared.params.items.len;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) err.AllocationError![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "<fn {s}>",
            .{self.declared.name.lexeme}
        ) catch return err.outOfMemory();
    }
};


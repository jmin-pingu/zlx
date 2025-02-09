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

// NOTE:
// What if we do the following
// Make env store callableType
// and let callableType be
//
// pub const CallableType = union(enum) {
//     Declared: stmt.Function,
//     Native: *Callable(),
// };
//
//
// TODO: need the idea of a "trait bound" here

pub const Function = struct {
    const Self = @This();
    declared: stmt.Function, 
    closure: *Environment,

    pub fn init(declared: stmt.Function, closure: *Environment) AllocationError!Function {
        return Function{
            .declared = declared,
            .closure = closure
        };
    }

    // pub fn initCallable(self: *Self) Callable() {
    //     return Callable().init(self);
    // }
    
    pub fn call(self: *Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) FunctionError!Object {
        var environment = try allocator.create(Environment);
        environment.* = Environment.init(allocator, self.closure);

        for (0..self.declared.params.items.len) |i| {
            try environment.define(self.declared.params.items[i].literal.Identifier, arguments.items[i], allocator); // should return a variable error
        }

        const stmtValue = interpreter.executeBlock(self.declared.body, environment) catch {
            return FunctionError.FunctionCallError;
        };

        if (stmtValue != null) {
            return stmtValue.?;
        } else {
            // Nothing returns is default
            return Object{.Nil=null};
        }
    }

    pub fn arity(self: *Self) usize {
        return self.declared.params.items.len;
    }

    pub fn toString(self: *Self, allocator: std.mem.Allocator) AllocationError![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "<fn {s}>",
            .{self.declared.name.lexeme}
        ) catch return err.outOfMemory();
    }
};


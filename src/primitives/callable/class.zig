const std = @import("std");
const Interpreter = @import("../../interpreter.zig").Interpreter;
const ArrayList = std.ArrayList;
const Object = @import("../object.zig").Object;

const err = @import("../../error.zig");

pub const Class = struct {
    const Self = @This();
    name: []const u8,
    // NOTE: is there a way to make this implement Callable?
    // Callable{ .Native = panic.initCallable()};
    pub fn init(name: []const u8) Class {
        return Class{
            .name=name
        };
    }
    
    pub fn call(self: Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) err.FunctionError!Object {
        _ = allocator;
        _ = interpreter;
        _ = arguments;
        _ = self;
        // Creates an instance
        return Object{.Nil=null};
    }

    pub fn arity(self: Self) usize {
        _ = self;
        return 0;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) err.AllocationError![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "<class {s}>",
            .{self.name}
        ) catch return err.outOfMemory();
    }
};


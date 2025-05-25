const std = @import("std");
const Instance = @import("instance.zig").Instance;
const Callable = @import("callable.zig").Callable;
const Function = @import("function.zig").Function;
const Interpreter = @import("../../interpreter.zig").Interpreter;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Object = @import("../object.zig").Object;

const err = @import("../../error.zig");

pub const Class = struct {
    const Self = @This();
    name: []const u8,
    methods: StringHashMap(Function),
    // NOTE: is there a way to make this implement Callable?
    // Callable{ .Native = panic.initCallable()};
    pub fn init(name: []const u8, methods: StringHashMap(Function)) Class {
        return Class{
            .name=name,
            .methods=methods,
        };
    }
    
    pub fn call(self: Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) err.FunctionError!Object {
        _ = interpreter;
        _ = arguments;
        // Creates an instance
        const instance_ref = allocator.create(Callable) catch return err.outOfMemory();
        instance_ref.* = Callable{ .Instance = try Instance.init(self, allocator) };
        return Object{ .Instance = instance_ref };
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


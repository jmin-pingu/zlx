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
    superclass: ?Object,
    methods: StringHashMap(Object),
    // NOTE: is there a way to make this implement Callable?
    // Callable{ .Native = panic.initCallable()};
    pub fn init(name: []const u8, superclass: ?Object, methods: StringHashMap(Object)) Class {
        return Class{
            .name=name,
            .superclass=superclass,
            .methods=methods,
        };
    }
    
    pub fn call(self: Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) err.FunctionError!Object {
        // NOTE: need to reevaluate the Boxing strategy
        // Creates an instance
        const class_ref = allocator.create(Class) catch return err.outOfMemory();
        class_ref.* = self;
        const instance_ref = allocator.create(Callable) catch return err.outOfMemory();
        const instance = try Instance.init(class_ref, allocator);
        instance_ref.* = Callable{ .Instance = instance };
        
        if (self.findMethod("init")) |initializer| {
            const bound_instance =  initializer.bind(instance, allocator) catch return err.FunctionError.FunctionCallError;
            _ = try bound_instance.Function.call(interpreter, arguments, allocator);
        }

        return Object{ .Instance = instance_ref };
    }

    pub fn arity(self: Self) usize {
        if (self.findMethod("init")) |initializer| {
            return initializer.Function.arity();
        }
        return 0;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) err.AllocationError![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "<class {s}>",
            .{self.name}
        ) catch return err.outOfMemory();
    }

    pub fn findMethod(self: Self, name: []const u8) ?Object {
        if (self.methods.get(name)) |method| {
            return method;
        }

        if (self.superclass) |superclass| {
            switch (superclass) {
                .Class => |callable| { 
                    switch (callable.*) {
                        .Class => |class| { return class.findMethod(name);},
                        else => { return null; },
                    }
                },
                else => {return null;}
            }
        }

        return null;
    }
};


const std = @import("std");
const Class = @import("class.zig").Class;
 
const Object = @import("../object.zig").Object;
const Token = @import("../token.zig").Token;

const err = @import("../../error.zig");
const Interpreter = @import("../../interpreter.zig").Interpreter;

const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

pub const Instance = struct {
    class: *Class,
    fields: StringHashMap(Object),
 
    pub fn init(class: *Class, allocator: std.mem.Allocator) err.AllocationError!*Instance {
        const instance_ref = try allocator.create(Instance);
        instance_ref.* = Instance { 
            .class = class,
            .fields = StringHashMap(Object).init(allocator),
        };
        return instance_ref;
    }

    const Self = @This();

    pub fn toString(self: Self, allocator: std.mem.Allocator) err.AllocationError![] const u8 {
        return std.fmt.allocPrint(
            allocator,
            "<instance {s}>",
            .{self.class.name}
        ) catch return err.outOfMemory();
    }

    pub fn call(self: Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) err.FunctionError!Object {
        return self.class.call(interpreter, arguments, allocator);
    }

    pub fn arity(self: Self) usize {
        return self.class.arity();
    }

    pub fn get(self: Self, name: Token, allocator: std.mem.Allocator) err.RuntimeError!Object {
        if (self.fields.get(name.lexeme)) |value| {
            return value;
        } else if (self.class.findMethod(name.lexeme)) |method| {
            const instance_ref = try allocator.create(Instance);
            instance_ref.* = self;
            return try method.bind(instance_ref, allocator);
        } else {
            const error_message = std.fmt.allocPrint(
                allocator,
                "Undefined property '{s}'.",
                .{name.lexeme}
            ) catch return err.outOfMemory();
        return err.errorMessage(err.RuntimeError, name.line, error_message, err.RuntimeError.UndefinedProperty, allocator);
        }
    }

    pub fn set(self: *Self, name: Token, value: Object) err.RuntimeError!void {
        return self.fields.put(name.lexeme, value) catch err.outOfMemory();
    }
};

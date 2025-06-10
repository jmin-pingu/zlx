const std = @import("std");
const ArrayList = std.ArrayList;
const Object = @import("../object.zig").Object;
const CallableInterface = @import("callable.zig").CallableInterface;
const err = @import("../../error.zig");
const Interpreter = @import("../../interpreter.zig").Interpreter;

pub const Clock = struct {
    const S = @This();

    pub fn arity(self: *S) usize {
        _ = self;
        return 0;
    }

    pub fn call(self: *S, interpreter: *Interpreter, arguments: ArrayList(Object), alloc: std.mem.Allocator) err.FunctionError!Object {
        _ = alloc;
        _ = self;
        _ = interpreter;
        _ = arguments;
        return Object{.Number = @floatFromInt(@divFloor(std.time.milliTimestamp(), 1000))};
    }

    pub fn toString(self: *S, alloc: std.mem.Allocator) err.AllocationError![]const u8{
        _ = self;
        _ = alloc;
        return "<native fn>";
    }

    pub fn initCallable(self: *S) CallableInterface() {
        return CallableInterface().init(self);
    }
};

pub const Panic = struct {
    const S = @This();

    pub fn arity(self: *S) usize {
        _ = self;
        return 0;
    }

    pub fn call(self: *S, interpreter: *Interpreter, arguments: ArrayList(Object), alloc: std.mem.Allocator) err.FunctionError!Object {
        _ = alloc;
        _ = self;
        _ = interpreter;
        _ = arguments;
        @panic("panicked");
    }

    pub fn toString(self: *S, alloc: std.mem.Allocator) err.AllocationError![]const u8{
        _ = self;
        _ = alloc;
        return "<native fn>";
    }

    pub fn initCallable(self: *S) CallableInterface() {
        return CallableInterface().init(self);
    }
};

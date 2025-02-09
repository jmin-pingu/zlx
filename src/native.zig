const std = @import("std");
const ArrayList = std.ArrayList;

const FunctionType = @import("token/object.zig").FunctionType;
const Object = @import("token/object.zig").Object;

const Interpreter = @import("interpreter.zig").Interpreter;
const Callable = @import("callable.zig").Callable;

const err = @import("error.zig");
const FunctionError = err.FunctionError;
const AllocationError = err.AllocationError;

pub const Clock = struct {
    const S = @This();

    pub fn arity(self: *S) usize {
        _ = self;
        return 0;
    }

    pub fn call(self: *S, interpreter: *Interpreter, arguments: ArrayList(Object), alloc: std.mem.Allocator) FunctionError!Object {
        _ = alloc;
        _ = self;
        _ = interpreter;
        _ = arguments;
        std.debug.print("{d}\n", .{@divFloor(std.time.milliTimestamp(), 1000)});
        return Object{.Nil = null};
    }

    pub fn toString(self: *S, alloc: std.mem.Allocator) AllocationError![]const u8{
        _ = self;
        _ = alloc;
        return "<native fn>";
    }

    pub fn initCallable(self: *S) Callable() {
        return Callable().init(self);
    }
};

pub const Panic = struct {
    const S = @This();

    pub fn arity(self: *S) usize {
        _ = self;
        return 0;
    }

    pub fn call(self: *S, interpreter: *Interpreter, arguments: ArrayList(Object), alloc: std.mem.Allocator) FunctionError!Object {
        _ = alloc;
        _ = self;
        _ = interpreter;
        _ = arguments;
        @panic("panicked");
    }

    pub fn toString(self: *S, alloc: std.mem.Allocator) AllocationError![]const u8{
        _ = self;
        _ = alloc;
        return "<native fn>";
    }

    pub fn initCallable(self: *S) Callable() {
        return Callable().init(self);
    }
};

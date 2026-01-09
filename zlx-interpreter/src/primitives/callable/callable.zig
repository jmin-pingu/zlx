const std = @import("std");
const Class = @import("class.zig").Class;
const Function = @import("function.zig").Function;
const Instance = @import("instance.zig").Instance;
const Object = @import("../object.zig").Object;
const Expr = @import("../expr.zig").Expr;
const stmt = @import("../stmt.zig");

const Interpreter = @import("../../interpreter.zig").Interpreter;
const AllocationError = @import("../../error.zig").AllocationError;
const FunctionError = @import("../../error.zig").FunctionError;

const ArrayList = std.ArrayList;

pub const Callable = union(enum) {
    Declared: Function,
    Native: CallableInterface(),
    Class: Class,
    Instance: *Instance,

    const Self = @This();

    pub fn arity(self: Self) usize {
        switch (self) {
            inline else => |case| return case.arity(),
        }
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) AllocationError![]const u8 {
        switch (self) {
            inline else => |case| return case.toString(allocator),
        }
    }

    pub fn call(self: Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) FunctionError!Object {
        switch (self) {
            inline else => |case| return case.call(interpreter, arguments, allocator),
        }
    }
};

pub fn CallableInterface() type {
    return struct {
        const T = FunctionError!Object;
        const Self = @This();
        ptr: *anyopaque,
        callFn: *const fn (*anyopaque, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) T,
        arityFn: *const fn (*anyopaque) usize,
        toStringFn: *const fn (*anyopaque, std.mem.Allocator) AllocationError![]const u8,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .pointer) @compileError("ptr must be a pointer");
            if (ptr_info.pointer.size != .one) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn callImpl(pointer: *anyopaque, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.call, .{self, interpreter, arguments, allocator});
                }

                pub fn arityImpl(pointer: *anyopaque) usize {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.arity, .{self});
                }

                pub fn toStringImpl(pointer: *anyopaque, allocator: std.mem.Allocator) AllocationError![]const u8 {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.pointer.child.toString, .{self, allocator});
                }
            };

            return .{
                .ptr= ptr,
                .callFn= gen.callImpl,
                .toStringFn= gen.toStringImpl,
                .arityFn= gen.arityImpl,
            };
        }

        pub inline fn call(self: Self, interpreter: *Interpreter, arguments: ArrayList(Object), allocator: std.mem.Allocator) T {
            return self.callFn(self.ptr, interpreter, arguments, allocator);
        }

        pub inline fn arity(self: Self) usize {
            return self.arityFn(self.ptr);
        }

        pub inline fn toString(self: Self, allocator: std.mem.Allocator) AllocationError![]const u8 {
            return self.toStringFn(self.ptr, allocator);
        }

        pub fn implementedBy(implementor: type) bool {
            if (@typeInfo(implementor) != .Struct) return false;
            inline for (@typeInfo(Self).Struct.decls) |declaration| {
                if (@typeInfo(@TypeOf(@field(Self, declaration.name))).Fn.calling_convention == .Inline) {
                    if (!std.meta.hasMethod(implementor, declaration.name)) return false;
                    const implteeMethodTypeInfo = @typeInfo(@TypeOf(@field(Self, declaration.name))).Fn; 
                    const impltorMethodTypeInfo = @typeInfo(@TypeOf(@field(implementor, declaration.name))).Fn; 
                    if (impltorMethodTypeInfo.params.len != implteeMethodTypeInfo.params.len) return false;
                    inline for (1..impltorMethodTypeInfo.params.len) |k| {
                        if (implteeMethodTypeInfo.params[k].type.? != impltorMethodTypeInfo.params[k].type.?) return false;
                    }
                }
            }
            return true;
        }
    };
}
 

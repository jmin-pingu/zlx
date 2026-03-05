const std = @import("std");
const Metadata = @import("gc.zig").Metadata;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;
const Chunk = @import("chunk.zig").Chunk;
const Object = @import("object.zig").Object;
const ObjectType = @import("object.zig").ObjectType;
const Function = @import("object.zig").Function;
const NativeFunctionType = @import("object.zig").NativeFunctionType;
const NativeFunction = @import("object.zig").NativeFunction;
const String = @import("object.zig").String;
const ArrayList = std.ArrayList;

const ValueTag = enum {
    Bool,
    Number,
    Nil,
    Object
};

pub const Value = union(ValueTag) {
    Bool: bool,
    Number: f64,
    Nil: void,
    Object: *Object,

    pub fn initBool(boolean: bool) Value {
        return .{.Bool = boolean};
    }

    pub fn initFunction(function: *Function) !Value {
        const object = function.toObject();
        return .{ .Object = object };
    }

    pub fn initNativeFunction(allocator: std.mem.Allocator, metadata: *Metadata, nativeFn: NativeFunctionType, arity: usize) !Value {
        const native = try NativeFunction.initNativeFunction(allocator, metadata, nativeFn, arity);
        const object = native.toObject();
        return .{ .Object = object };
    }


    pub fn initString(value: []const u8, metadata: *Metadata, allocator: std.mem.Allocator) !Value {
        if (metadata.retrieveString(value)) |object| {
            return .{ .Object = object };
        } else {
            const string = try String.initString(value, metadata, allocator);
            const object = string.toObject();
            try metadata.setString(value, object);
            return .{ .Object = object };
        }
    }

    pub fn initNumber(number: f64) Value {
        return .{.Number = number};
    }

    pub fn initNil() Value {
        return .{.Nil = {}};
    }

    pub fn isValueTag(self: Value, valueTag: ValueTag) bool {
        return self == valueTag;
    }

    pub fn isObjectType(self: Value, objectType: ObjectType) bool {
        if (self != .Object) {
            return false;
        }
        return self.Object.objectType == objectType;
    }

    pub fn isObject(self: Value) bool {
        return self == .Object;
    }

    pub fn isFalsey(self: Value) bool {
        return self.isValueTag(.Nil) or (self.isValueTag(.Bool) and !self.Bool);
    }

    pub fn isEqual(self: Value, other: *const Value) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other.*)) {
             return false;
        }

        return switch (self) {
            .Bool => return self.Bool == other.Bool,
            .Number => return self.Number == other.Number,
            .Object => {
                if (self.Object.objectType != other.*.Object.objectType) {
                    return false;
                }
                switch (self.Object.objectType) {
                    .String => {
                        return self.Object.toObjectType(String) == other.Object.toObjectType(String);
                    },
                    .Function => {
                        return self.Object.toObjectType(Function) == other.Object.toObjectType(Function);
                    },
                    .NativeFunction => {
                        return self.Object.toObjectType(NativeFunction) == other.Object.toObjectType(NativeFunction);
                    },
                }
            },
            .Nil => true,
        };
    }

    pub fn print(self: Value) void {
        switch (self) {
            .Number => std.debug.print("{d}", .{self.Number}),
            .Bool => std.debug.print("{any}", .{self.Bool}),
            .Nil => std.debug.print("nil", .{}),
            .Object => {
                switch (self.Object.objectType) {
                    .String => {
                        const string: *String = self.Object.toObjectType(String);
                        std.debug.print("\"{s}\"", .{string.value});
                    },
                    .Function => {
                        const function: *Function = self.Object.toObjectType(Function);
                        if (function.name) |name| {
                            std.debug.print("<fn {s}>", .{name.value});
                        } else {
                            std.debug.print("<script>", .{});
                        }
                    },
                    .NativeFunction => std.debug.print("<native_fn>", .{}),
                }
            }
        }
    }

    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .Object => {
                switch (self.Object.objectType) {
                    .String => {
                        const string: *String = self.Object.toObjectType();
                        string.deinit(allocator);
                    },
                }
            },
            else => {}
        }
    }
};

test "values initialization" {
    const Compiler = @import("compiler.zig").Compiler;
    const allocator = std.testing.allocator;
    const compiler = try allocator.create(Compiler);
    const metadata = try allocator.create(Metadata);
    metadata.* = Metadata.init(allocator); 
    compiler.* = try Compiler.init(metadata, .Script, null, allocator);
    defer metadata.trace(null);
}

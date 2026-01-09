const std = @import("std");
const Metadata = @import("gc.zig").Metadata;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;
const Chunk = @import("chunk.zig").Chunk;

const ObjectType = enum {
    String,
    Function
};

pub const Object = struct {
    objectType: ObjectType,
    next: ?*Object,
    
    pub fn toObjectType(self: *Object) *switch (self.objectType) {
        .String => String,
    } {
        return @ptrCast(@alignCast(self));
    }

    pub fn deinit(self: *Object, allocator: std.mem.Allocator) void {
        var curr: ?*Object = self;
        while (curr) |obj| {
            curr = obj.next;
            obj.toObjectType().deinit(allocator);
        }
    }
};

/// Heap allocated objects
pub const String = struct {
    object: Object,
    value: []u8,

    pub fn toObject(self: *String) *Object {
        return @ptrCast(@alignCast(self));
    }
    
    pub fn initString(value: []const u8, metadata: *Metadata, allocator: std.mem.Allocator) !*String {
        const string = try allocator.create(String);
        const value_ptr = try allocator.alloc(u8, value.len);
        @memcpy(value_ptr, value);
        string.* = .{ .object = .{ .objectType = .String, .next = metadata.allocations }, .value = value_ptr };
        metadata.allocations = string.toObject();
        return string;
    }
    
    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.value);
        allocator.destroy(self);
    }
};

pub const Function = struct {
    object: Object,
    arity: usize,
    chunk: *const Chunk,
    name: *String,

    pub fn toObject(self: *String) *Object {
        return @ptrCast(@alignCast(self));
    }
    
    pub fn initFunction(allocator: std.mem.Allocator, metadata: *Metadata) !*Function {
        const function = try allocator.create(Function);
        const chunk = Chunk.init(allocator);
        function.* = .{ 
            .object = .{ .objectType = .Function, .next = metadata.allocations }, 
            .arity = 0, 
            .chunk = chunk, 
            .name = undefined
        };
        // NOTE: do I need to add to allocations?
        return function;
    }
    
    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.value);
        allocator.destroy(self);
    }
};

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
                        return self.Object.toObjectType() == other.Object.toObjectType();
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
                        const string: *String = self.Object.toObjectType();
                        std.debug.print("{s}", .{string.value});
                    },
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

test "value" {
    const allocator = std.testing.allocator;
    const s: Value = try Value.initString("test", null, allocator);
    defer s.deinit(allocator);
}

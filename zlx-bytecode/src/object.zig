const std = @import("std");
const Value = @import("value.zig").Value;
const Metadata = @import("gc.zig").Metadata;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;
const Chunk = @import("chunk.zig").Chunk;
const ArrayList = std.ArrayList;

pub const ObjectType = enum {
    String,
    Function,
    NativeFunction
};

pub const FunctionType = enum {
    Function,
    Script
};

// TODO: change all print use cases to use the Writer/Reader interface + io in stdlib
pub const Object = struct {
    objectType: ObjectType,
    next: ?*Object,
    
    pub fn toObjectType(self: *Object, comptime T: type) *T {
        switch (T) {
            String, Function, NativeFunction => {},
            else => @compileError("Unsupported type")
        }
        return @ptrCast(@alignCast(self));
    }
    
    pub fn print(self: *Object) void {
        switch (self.objectType) {
            .String => self.toObjectType(String).print(),
            .Function => self.toObjectType(Function).print(),
            .NativeFunction => self.toObjectType(NativeFunction).print(),
        }

    }

    pub fn deinit(self: *Object, allocator: std.mem.Allocator) void {
        var curr: ?*Object = self;
        while (curr) |obj| {
            curr = obj.next;
            switch (obj.objectType) {
                .String => obj.toObjectType(String).deinit(allocator),
                .Function => obj.toObjectType(Function).deinit(allocator),
                .NativeFunction => obj.toObjectType(NativeFunction).deinit(allocator),
            }
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

    pub fn print(self: String) void {
        std.debug.print("{s}", .{self.value});
    }
    
    pub fn initString(value: []const u8, metadata: *Metadata, allocator: std.mem.Allocator) !*String {
        if (metadata.retrieveString(value)) |object| {
            return object.toObjectType(String);
        } 
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
    chunk: *Chunk,
    name: ?*String,

    pub fn toObject(self: *Function) *Object {
        return @ptrCast(@alignCast(self));
    }

    pub fn toValue(self: *Function) Value {
        const obj: *Object = @ptrCast(@alignCast(self));
        return Value{.Object = obj};
    }

    pub fn print(self: Function) void {
        if (self.name) |name| {
            std.debug.print("<", .{});
            name.print();
            std.debug.print(">", .{});
        } else {
            std.debug.print("<script>", .{});
        }
            
        std.debug.print("[{d}]", .{self.arity});
    }
    
    pub fn initFunction(allocator: std.mem.Allocator, metadata: *Metadata, name: ?*String) !*Function {
        const function = try allocator.create(Function);
        const chunkPtr = try allocator.create(Chunk);
        chunkPtr.* = Chunk.init(allocator);
        function.* = .{ 
            .object = .{ .objectType = .Function, .next = metadata.allocations }, 
            .arity = 0, 
            .chunk = chunkPtr, 
            .name = name
        };
        // NOTE: do I need to add to allocations?
        return function;
    }

    pub fn deinit(self: *Function, allocator: std.mem.Allocator) void {
        if (self.name) |name| name.deinit(allocator);
        allocator.destroy(self);
        self.chunk.deinit(allocator);
    }
};

pub const NativeFunctionType = *const fn ([*]Value) Value;
pub const NativeFunction = struct {
    object: Object,
    nativeFn: NativeFunctionType,
    arity: usize,

    pub fn toObject(self: *NativeFunction) *Object {
        return @ptrCast(@alignCast(self));
    }

    pub fn toValue(self: *NativeFunction) Value {
        const obj: *Object = @ptrCast(@alignCast(self));
        return Value{.Object = obj};
    }

    pub fn print(self: NativeFunction) void {
        _ = self;
        std.debug.print("<native_fn>", .{});
    }
    
    pub fn initNativeFunction(allocator: std.mem.Allocator, metadata: *Metadata, nativeFn: NativeFunctionType, arity: usize) !*NativeFunction {
        const native = try allocator.create(NativeFunction);
        const chunkPtr = try allocator.create(Chunk);
        chunkPtr.* = Chunk.init(allocator);
        native.* = .{ 
            .object = .{ .objectType = .NativeFunction, .next = metadata.allocations }, 
            .arity = arity,
            .nativeFn = nativeFn,
        };
        // NOTE: do I need to add to allocations?
        return native;
    }

    pub fn deinit(self: *NativeFunction, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};


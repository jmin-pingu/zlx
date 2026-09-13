const std = @import("std");
const AllocatorError = std.mem.Allocator.Error;
const MemoryManager = @import("memory.zig").MemoryManager;
const Value = @import("value.zig").Value;
const Metadata = @import("gc.zig").Metadata;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;
const Chunk = @import("chunk.zig").Chunk;

pub const ObjectType = enum {
    String,
    Function,
    NativeFunction,
    Closure,
    Upvalue,
};

pub const FunctionType = enum {
    Function,
    Script
};

// TODO: change all print use cases to use the Writer/Reader interface + io in stdlib
pub const Object = struct {
    objectType: ObjectType,
    isMarked: bool ,
    next: ?*Object,

    fn TagFromType(comptime T: type) ObjectType {
        return switch (T) {
            String => .String,
            Function => .Function,
            Closure => .Closure,
            NativeFunction => .NativeFunction,
            Upvalue => .Upvalue,
            else => @compileError(@typeName(T) ++ " is not an Object type"),
        };
    }

    pub fn toObjectType(self: *Object, comptime T: type) *T {
        comptime if (!@hasField(T, "object")) @compileError(@typeName(T) ++ " is not an Object type");
        return @fieldParentPtr("object", self);
    }

    pub fn toObject(obj: anytype) *Object {
        const P = @TypeOf(obj);
        comptime {
            const info = @typeInfo(P);
            if (info != .pointer or !@hasField(info.pointer.child, "object"))
                @compileError(@typeName(P) ++ " is not a pointer to an Object type");
        }
        return &obj.object;
    }

    pub fn toValue(obj: anytype) Value {
        const P = @TypeOf(obj);
        comptime {
            const info = @typeInfo(P);
            if (info != .pointer or !@hasField(info.pointer.child, "object"))
                @compileError(@typeName(P) ++ " is not a pointer to an Object type");
        }
        return .{ .Object = &obj.object };
    }

    fn TypeFromTag(comptime tag: ObjectType) type {
        return switch (tag) {
            .String => String,
            .Function => Function,
            .Closure => Closure,
            .NativeFunction => NativeFunction,
            .Upvalue => Upvalue,
        };
    }

    pub fn freeObject(self: *Object, allocator: std.mem.Allocator) void {
        switch (self.objectType) {
            inline else => |tag| self.toObjectType(Object.TypeFromTag(tag)).deinit(allocator)
        }
    }

    pub fn print(self: *Object) void {
        switch (self.objectType) {
            inline else => |tag| self.toObjectType(Object.TypeFromTag(tag)).print()
        }
    }

    pub fn initObject(comptime T: type, allocator: std.mem.Allocator) !*T {
        _ = TagFromType(T);
        return try allocator.create(T);
    }

    pub fn freeObjects(self: *Object, allocator: std.mem.Allocator) void {
        var curr: ?*Object = self;
        while (curr) |obj| {
            curr = obj.next;
            obj.freeObject(allocator);
        }
    }
};

/// Heap allocated objects
pub const String = struct {
    object: Object,
    value: []u8,

    pub fn print(self: String) void {
        std.debug.print("{s}", .{self.value});
    }

    pub fn init(allocator: std.mem.Allocator, memoryManager: *MemoryManager, value: []const u8, metadata: *Metadata) !*String {
        if (metadata.retrieveString(value)) |object| {
            return object.toObjectType(String);
        } 
        const string = try memoryManager.allocateObject(String, allocator);
        const value_ptr = try allocator.alloc(u8, value.len);
        @memcpy(value_ptr, value);
        string.* = .{ 
            .object = .{ 
                .objectType = .String, 
                .next = metadata.allocations,
                .isMarked = true
            },
            .value = value_ptr 
        };
        metadata.allocations = Object.toObject(string);
        return string;
    }
   
    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        allocator.free(self.value);
        allocator.destroy(self);
    }
};

pub const Closure = struct {
    object: Object,
    function: *Function,
    upvalues: []*Upvalue,
    upvalueCount: usize,

    pub fn print(self: Closure) void {
        self.function.print();
    }

    pub fn init (
        allocator: std.mem.Allocator,
        memoryManager: *MemoryManager,
        metadata: *Metadata, 
        function: *Function
    ) !*Closure {
            const closure = try memoryManager.allocateObject(Closure, allocator);
            const upvalues = try allocator.alloc(*Upvalue, function.upvalueCount);
            for (0..upvalues.len) |n| {
                upvalues[n] = undefined;
            }
    
            closure.* = .{ 
                .object = .{ 
                    .objectType = .Closure, 
                    .next = metadata.allocations,
                    .isMarked = false
                }, 
                .function = function, 
                .upvalues = upvalues,
                .upvalueCount = function.upvalueCount,
            };
            return closure;
        }
    
        pub fn deinit(self: *Closure, allocator: std.mem.Allocator) void {
            // NOTE: only free the closure, not the function. 
            // There can be many closures over a function
            allocator.free(self.upvalues);
            self.deinit(allocator);
        }
    };

pub const Function = struct {
    object: Object,
    arity: usize,
    upvalueCount: u8,
    chunk: *Chunk,
    name: ?*String,

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

    const InitFunctionSignature = fn (std.mem.Allocator, *Metadata, ?*String) AllocatorError!*Function;

    pub fn init(allocator: std.mem.Allocator, memoryManager: *MemoryManager, metadata: *Metadata, name: ?*String) !*Function {
        const function = try memoryManager.allocateObject(Function, allocator);
        const chunkPtr = try allocator.create(Chunk);
        chunkPtr.* = Chunk.init(allocator);
        function.* = .{ 
            .object = .{ 
                .objectType = .Function, 
                .next = metadata.allocations,
                .isMarked = false
            }, 
            .arity = 0, 
            .upvalueCount = 0, 
            .chunk = chunkPtr, 
            .name = name
        };
        return function;
    }

    // NOTE: worried about deinit...
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

    pub fn print(self: NativeFunction) void {
        _ = self;
        std.debug.print("<native_fn>", .{});
    }
    
    pub fn init(allocator: std.mem.Allocator, memoryManager: *MemoryManager, metadata: *Metadata, nativeFn: NativeFunctionType, arity: usize) !*NativeFunction {
        const native = try memoryManager.allocateObject(NativeFunction, allocator);
        const chunkPtr = try allocator.create(Chunk);
        chunkPtr.* = Chunk.init(allocator);
        native.* = .{ 
            .object = .{ 
                .objectType = .NativeFunction, 
                .next = metadata.allocations,
                .isMarked = false
            }, 
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

pub const Upvalue = struct {
    object: Object,
    location: *const Value,
    closed: Value,
    next: ?*Upvalue,

    pub fn print(self: Upvalue) void {
        _ = self;
        std.debug.print("<upvalue>", .{});
    }

    pub fn init(allocator: std.mem.Allocator, memoryManager: *MemoryManager, metadata: *Metadata, slot: *const Value) !*Upvalue {
        const upvalue = try memoryManager.allocateObject(Upvalue, allocator);
        upvalue.* = .{ 
            .object = .{ 
                .objectType = .Upvalue, 
                .next = metadata.allocations,
                .isMarked = false
            }, 
            .location = slot,
            .closed = Value.initNil(),
            .next = null
        };
        return upvalue;
    }
    
    pub fn deinit(self: *Upvalue, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};



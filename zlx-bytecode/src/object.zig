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



const testing = std.testing;

fn testMemoryManager(allocator: std.mem.Allocator) !*MemoryManager {
    return MemoryManager.init(allocator, undefined, undefined);
}

fn nativeStub(args: [*]Value) Value {
    return args[0];
}

test "string init copies its value and links into the allocation list" {
    const allocator = testing.allocator;
    var metadata = Metadata.init(allocator);
    defer metadata.interned.deinit();
    defer metadata.identifiers.deinit();
    const mm = try testMemoryManager(allocator);
    defer allocator.destroy(mm);

    var source = [_]u8{ 'a', 'b', 'c' };
    const first = try String.init(allocator, mm, &source, &metadata);
    defer first.deinit(allocator);
    source[0] = 'z';
    try testing.expectEqualStrings("abc", first.value);
    try testing.expectEqual(ObjectType.String, first.object.objectType);
    try testing.expectEqual(Object.toObject(first), metadata.allocations.?);
    try testing.expect(first.object.next == null);

    const second = try String.init(allocator, mm, "def", &metadata);
    defer second.deinit(allocator);
    try testing.expectEqual(Object.toObject(second), metadata.allocations.?);
    try testing.expectEqual(Object.toObject(first), second.object.next.?);
}

test "string init returns the registered interned object" {
    const allocator = testing.allocator;
    var metadata = Metadata.init(allocator);
    defer metadata.interned.deinit();
    defer metadata.identifiers.deinit();
    const mm = try testMemoryManager(allocator);
    defer allocator.destroy(mm);

    const first = try String.init(allocator, mm, "dup", &metadata);
    defer first.deinit(allocator);
    try metadata.setString("dup", Object.toObject(first));

    const again = try String.init(allocator, mm, "dup", &metadata);
    try testing.expectEqual(first, again);
}

test "string init without interning creates distinct objects" {
    const allocator = testing.allocator;
    var metadata = Metadata.init(allocator);
    defer metadata.interned.deinit();
    defer metadata.identifiers.deinit();
    const mm = try testMemoryManager(allocator);
    defer allocator.destroy(mm);

    const a = try String.init(allocator, mm, "same", &metadata);
    defer a.deinit(allocator);
    const b = try String.init(allocator, mm, "same", &metadata);
    defer b.deinit(allocator);
    try testing.expect(a != b);
    try testing.expectEqualStrings(a.value, b.value);
}

test "object conversions round trip" {
    const allocator = testing.allocator;
    var metadata = Metadata.init(allocator);
    defer metadata.interned.deinit();
    defer metadata.identifiers.deinit();
    const mm = try testMemoryManager(allocator);
    defer allocator.destroy(mm);

    const string = try String.init(allocator, mm, "rt", &metadata);
    defer string.deinit(allocator);

    const object = Object.toObject(string);
    try testing.expectEqual(string, object.toObjectType(String));
    const value = Object.toValue(string);
    try testing.expectEqual(object, value.Object);
    try testing.expect(value.isObjectType(.String));
}

test "function init starts empty" {
    const allocator = std.heap.page_allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const name = try String.init(allocator, mm, "f", &metadata);
    const named = try Function.init(allocator, mm, &metadata, name);
    try testing.expectEqual(ObjectType.Function, named.object.objectType);
    try testing.expect(!named.object.isMarked);
    try testing.expectEqual(@as(usize, 0), named.arity);
    try testing.expectEqual(@as(u8, 0), named.upvalueCount);
    try testing.expectEqual(@as(usize, 0), named.chunk.code.items.len);
    try testing.expectEqual(@as(usize, 0), named.chunk.constants.values.items.len);
    try testing.expectEqual(name, named.name.?);

    const script = try Function.init(allocator, mm, &metadata, null);
    try testing.expect(script.name == null);
    try testing.expect(script.chunk != named.chunk);
}

test "closure init allocates one slot per function upvalue" {
    const allocator = std.heap.page_allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const function = try Function.init(allocator, mm, &metadata, null);
    function.upvalueCount = 3;
    const closure = try Closure.init(allocator, mm, &metadata, function);
    try testing.expectEqual(ObjectType.Closure, closure.object.objectType);
    try testing.expectEqual(function, closure.function);
    try testing.expectEqual(@as(usize, 3), closure.upvalues.len);
    try testing.expectEqual(@as(usize, 3), closure.upvalueCount);

    const plain = try Function.init(allocator, mm, &metadata, null);
    const noUpvalues = try Closure.init(allocator, mm, &metadata, plain);
    try testing.expectEqual(@as(usize, 0), noUpvalues.upvalues.len);
}

test "native function init stores arity and callback" {
    const allocator = std.heap.page_allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const native = try NativeFunction.init(allocator, mm, &metadata, nativeStub, 1);
    try testing.expectEqual(ObjectType.NativeFunction, native.object.objectType);
    try testing.expectEqual(@as(usize, 1), native.arity);

    var args = [_]Value{Value.initNumber(7)};
    try testing.expectEqual(@as(f64, 7), native.nativeFn(&args).Number);
}

test "upvalue init points at the slot and starts closed over nil" {
    const allocator = testing.allocator;
    var metadata = Metadata.init(allocator);
    defer metadata.interned.deinit();
    defer metadata.identifiers.deinit();
    const mm = try testMemoryManager(allocator);
    defer allocator.destroy(mm);

    var slot = Value.initNumber(3);
    const upvalue = try Upvalue.init(allocator, mm, &metadata, &slot);
    defer upvalue.deinit(allocator);

    try testing.expectEqual(ObjectType.Upvalue, upvalue.object.objectType);
    try testing.expectEqual(@as(*const Value, &slot), upvalue.location);
    try testing.expectEqual(@as(f64, 3), upvalue.location.*.Number);
    try testing.expect(upvalue.closed.isValueTag(.Nil));
    try testing.expect(upvalue.next == null);

    slot = Value.initNumber(4);
    try testing.expectEqual(@as(f64, 4), upvalue.location.*.Number);
}

test "object print dispatches on the object type" {
    const allocator = std.heap.page_allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const string = try String.init(allocator, mm, "p", &metadata);
    const named = try Function.init(allocator, mm, &metadata, string);
    const script = try Function.init(allocator, mm, &metadata, null);
    const native = try NativeFunction.init(allocator, mm, &metadata, nativeStub, 1);
    const closure = try Closure.init(allocator, mm, &metadata, named);
    var slot = Value.initNil();
    const upvalue = try Upvalue.init(allocator, mm, &metadata, &slot);

    Object.toObject(string).print();
    Object.toObject(named).print();
    Object.toObject(script).print();
    Object.toObject(native).print();
    Object.toObject(closure).print();
    Object.toObject(upvalue).print();
}

test "freeObjects releases every string in the allocation chain" {
    const allocator = testing.allocator;
    var metadata = Metadata.init(allocator);
    defer metadata.interned.deinit();
    defer metadata.identifiers.deinit();
    const mm = try testMemoryManager(allocator);
    defer allocator.destroy(mm);

    _ = try String.init(allocator, mm, "one", &metadata);
    _ = try String.init(allocator, mm, "two", &metadata);
    _ = try String.init(allocator, mm, "three", &metadata);

    metadata.allocations.?.freeObjects(allocator);
}

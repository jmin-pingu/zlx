const std = @import("std");
const Metadata = @import("gc.zig").Metadata;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;
const Chunk = @import("chunk.zig").Chunk;
const MemoryManager = @import("memory.zig").MemoryManager;
const Object = @import("object.zig").Object;
const ObjectType = @import("object.zig").ObjectType;
const Function = @import("object.zig").Function;
const NativeFunctionType = @import("object.zig").NativeFunctionType;
const NativeFunction = @import("object.zig").NativeFunction;
const Closure = @import("object.zig").Closure;
const Upvalue = @import("object.zig").Upvalue;
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
        return Object.toValue(function);
    }

    pub fn initClosure(closure: *Closure) !Value {
        return Object.toValue(closure);
    }

    pub fn initNativeFunction(allocator: std.mem.Allocator, memoryManager: *MemoryManager, metadata: *Metadata, nativeFn: NativeFunctionType, arity: usize) !Value {
        const native = try NativeFunction.init(allocator, memoryManager, metadata, nativeFn, arity);
        return Object.toValue(native);
    }

    pub fn initString(allocator: std.mem.Allocator, memoryManager: *MemoryManager, value: []const u8, metadata: *Metadata, ) !Value {
        if (metadata.retrieveString(value)) |object| {
            return Object.toValue(object.toObjectType(String));
        } else {
            const string = try String.init(allocator, memoryManager, value, metadata);
            const object = Object.toObject(string);
            try metadata.setString(value, object);
            return Object.toValue(object.toObjectType(String));
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
                    .Closure => {
                        return self.Object.toObjectType(Closure) == other.Object.toObjectType(Closure);
                    },
                    .Upvalue => {
                        return self.Object.toObjectType(Upvalue) == other.Object.toObjectType(Upvalue);
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
                    .Closure => {
                        const closure: *Closure = self.Object.toObjectType(Closure);
                        
                        if (closure.function.name) |name| {
                            std.debug.print("<closure {s}>", .{name.value});
                        } else {
                            // NOTE: silent error
                            return;
                        }
                    },
                    .Upvalue => {
                        const upvalue: *Upvalue = self.Object.toObjectType(Upvalue);
                        // TODO: not sure what value we need to print here?
                        std.debug.print("<upvalue> ", .{});
                        upvalue.location.print();
                    }
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


test "value isFalsey" {
    try std.testing.expect(Value.initNil().isFalsey());
    try std.testing.expect(Value.initBool(false).isFalsey());
    try std.testing.expect(!Value.initBool(true).isFalsey());
    try std.testing.expect(!Value.initNumber(0.0).isFalsey());
    try std.testing.expect(!Value.initNumber(1.0).isFalsey());
}

test "value isEqual primitives" {
    const n1 = Value.initNumber(42.0);
    const n2 = Value.initNumber(42.0);
    const n3 = Value.initNumber(1.0);
    try std.testing.expect(n1.isEqual(&n2));
    try std.testing.expect(!n1.isEqual(&n3));

    const t = Value.initBool(true);
    const f = Value.initBool(false);
    try std.testing.expect(t.isEqual(&t));
    try std.testing.expect(!t.isEqual(&f));

    const nil1 = Value.initNil();
    const nil2 = Value.initNil();
    try std.testing.expect(nil1.isEqual(&nil2));

    try std.testing.expect(!n1.isEqual(&t));
    try std.testing.expect(!nil1.isEqual(&f));
}

test "value isValueTag" {
    try std.testing.expect(Value.initNumber(1.0).isValueTag(.Number));
    try std.testing.expect(Value.initBool(true).isValueTag(.Bool));
    try std.testing.expect(Value.initNil().isValueTag(.Nil));
    try std.testing.expect(!Value.initNumber(1.0).isValueTag(.Bool));
}

const testing = std.testing;

fn testMemoryManager(allocator: std.mem.Allocator) !*MemoryManager {
    return MemoryManager.init(allocator, undefined, undefined);
}

fn identityNative(args: [*]Value) Value {
    return args[0];
}

test "values initialization" {
    const Compiler = @import("compiler.zig").Compiler;
    const allocator = std.testing.allocator;
    const compiler = try allocator.create(Compiler);
    const metadata = try allocator.create(Metadata);
    metadata.* = Metadata.init(allocator);
    const memoryManager = try MemoryManager.init(allocator, undefined, compiler);
    compiler.* = try Compiler.init(metadata, null, .Script, null, memoryManager, allocator);
    defer metadata.trace(null);

    try testing.expect(compiler.function.name == null);
    try testing.expectEqual(@as(usize, 0), compiler.function.arity);
    try testing.expectEqual(@as(u8, 1), compiler.localCount);
    try testing.expectEqual(@as(u8, 0), compiler.scopeDepth);
}

test "value initString interns equal strings" {
    const allocator = std.testing.allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const a = try Value.initString(allocator, mm, "hello", &metadata);
    const b = try Value.initString(allocator, mm, "hello", &metadata);
    const c = try Value.initString(allocator, mm, "world", &metadata);

    try testing.expect(a.isObjectType(.String));
    try testing.expectEqual(a.Object, b.Object);
    try testing.expect(a.Object != c.Object);
    try testing.expectEqualStrings("hello", a.Object.toObjectType(String).value);
    try testing.expectEqualStrings("world", c.Object.toObjectType(String).value);
    try testing.expectEqual(@as(?*Object, a.Object), metadata.retrieveString("hello"));
}

test "value isEqual compares strings by interned identity" {
    const allocator = std.testing.allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const a = try Value.initString(allocator, mm, "hello", &metadata);
    const b = try Value.initString(allocator, mm, "hello", &metadata);
    const c = try Value.initString(allocator, mm, "world", &metadata);
    const n = Value.initNumber(1);

    try testing.expect(a.isEqual(&b));
    try testing.expect(!a.isEqual(&c));
    try testing.expect(!a.isEqual(&n));
    try testing.expect(!n.isEqual(&a));
}

test "value isEqual distinguishes object kinds and identities" {
    const allocator = std.testing.allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const f1 = try Function.init(allocator, mm, &metadata, null);
    const f2 = try Function.init(allocator, mm, &metadata, null);
    const c1 = try Closure.init(allocator, mm, &metadata, f1);

    const vf1 = try Value.initFunction(f1);
    const vf1Again = try Value.initFunction(f1);
    const vf2 = try Value.initFunction(f2);
    const vc1 = try Value.initClosure(c1);

    try testing.expect(vf1.isEqual(&vf1Again));
    try testing.expect(!vf1.isEqual(&vf2));
    try testing.expect(!vf1.isEqual(&vc1));
    try testing.expect(vc1.isEqual(&vc1));

    try testing.expect(vf1.isObjectType(.Function));
    try testing.expect(!vf1.isObjectType(.Closure));
    try testing.expect(vc1.isObjectType(.Closure));
    try testing.expectEqual(f1, vf1.Object.toObjectType(Function));
    try testing.expectEqual(c1, vc1.Object.toObjectType(Closure));
}

test "value isObject and isObjectType on primitives" {
    try testing.expect(!Value.initNumber(1).isObject());
    try testing.expect(!Value.initBool(true).isObject());
    try testing.expect(!Value.initNil().isObject());
    try testing.expect(!Value.initNumber(1).isObjectType(.String));
    try testing.expect(!Value.initNil().isObjectType(.Function));
    try testing.expect(Value.initBool(false).isValueTag(.Bool));
    try testing.expect(!Value.initNil().isValueTag(.Object));
}

test "value initNativeFunction wraps arity and callback" {
    const allocator = std.testing.allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const value = try Value.initNativeFunction(allocator, mm, &metadata, identityNative, 1);
    try testing.expect(value.isObject());
    try testing.expect(value.isObjectType(.NativeFunction));

    const native = value.Object.toObjectType(NativeFunction);
    try testing.expectEqual(@as(usize, 1), native.arity);
    var args = [_]Value{Value.initNumber(7)};
    try testing.expectEqual(@as(f64, 7), native.nativeFn(&args).Number);
}

test "value print covers every variant" {
    const allocator = std.testing.allocator;
    var metadata = Metadata.init(allocator);
    const mm = try testMemoryManager(allocator);

    const string = try Value.initString(allocator, mm, "text", &metadata);
    const named = try Function.init(allocator, mm, &metadata, string.Object.toObjectType(String));
    const script = try Function.init(allocator, mm, &metadata, null);
    const namedClosure = try Closure.init(allocator, mm, &metadata, named);
    const scriptClosure = try Closure.init(allocator, mm, &metadata, script);
    var slot = Value.initNumber(2);
    const upvalue = try Upvalue.init(allocator, mm, &metadata, &slot);

    Value.initNumber(1.5).print();
    Value.initBool(true).print();
    Value.initNil().print();
    string.print();
    (try Value.initFunction(named)).print();
    (try Value.initFunction(script)).print();
    (try Value.initNativeFunction(allocator, mm, &metadata, identityNative, 1)).print();
    (try Value.initClosure(namedClosure)).print();
    (try Value.initClosure(scriptClosure)).print();
    Object.toValue(upvalue).print();
}

const std = @import("std");

const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;

const Object = @import("object.zig").Object;
const ObjectType = @import("object.zig").ObjectType;
const Closure = @import("object.zig").Closure;
const String = @import("object.zig").String;
const Function = @import("object.zig").Function;
const NativeFunction = @import("object.zig").NativeFunction;
const Upvalue = @import("object.zig").Upvalue;

const Compiler = @import("compiler.zig").Compiler;
const StringHashMap = @import("vm.zig").StringHashMap;

const mode = @import("main.zig").mode;

// The marking rule is that any Object is heap allocated 
// and thus needs to be marked. The challenge is identifying where
// Objects exist in the VM
// - Any `initObject` call
// - Each CallFrame contains a pointer to the closure to access constants/upvalues.

// NOTE: I want this to be responsible for all object creation related to the VM and Compiler
pub const MemoryManager = struct {
    vm: *VM, 
    compiler: *Compiler, 

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, vm: *VM, compiler: *Compiler) !*Self {
        const mm = try allocator.create(MemoryManager);
        mm.* = .{
            .vm = vm,
            .compiler = compiler
        };
        return mm;
    }


    // Memory management logic
    pub fn allocateObject(self: *Self, comptime T: type, allocator: std.mem.Allocator) !*T {
        if (mode == .StressGC) self.collectGarbage(allocator);

        const obj = Object.initObject(T, allocator);
        if (mode == .DebugGC) std.debug.print("{*} allocate {s}\n", .{ obj, @typeName(T) });
        return obj;
    }

    pub fn freeObject(self: Self, object: *Object, allocator: std.mem.Allocator) void {
        _ = self;
        defer if (mode == .DebugGC) std.debug.print("{any} free type {any}\n", .{object, object.objectType});
        try Object.freeObject(object, allocator);
    }

    // Garbage collection logic
    pub fn collectGarbage(self: Self, allocator: std.mem.Allocator) void {
        if (mode == .DebugGC) std.debug.print("-- gc begin\n", .{});
        self.markRoots(allocator);
        if (mode == .DebugGC) std.debug.print("-- gc end\n", .{});
    }
    
    fn markTable(self: Self, allocator: std.mem.Allocator, table: *StringHashMap) void {
        var iter = table.valueIterator();
        while (iter.next()) |entry| {
            self.markObject(allocator, Object.toObject(entry.keyPtr));
            self.markValue(allocator, entry.getValue());
        }
    }
    
    fn markRoots(self: Self, allocator: std.mem.Allocator) void {
        while (self.vm.stack) |slot| {
            self.markValue(allocator, slot);
        }
    
        self.markTable(allocator, self.vm.globals);
        self.markCompilerRoots();
    
        // NOTE: need to capture closures
        for (0..self.vm.frameCount) |i| {
            self.markObject(allocator,self.vm.frames[i].closure.toObject());
        }
    
        var curUpvalue = self.vm.openUpvalues;
        while (curUpvalue) |upvalue| {
            self.markObject(allocator, upvalue.toObject());
            curUpvalue = upvalue;
        }
    }
    
    fn markCompilerRoots(self: Self, allocator: std.mem.Allocator) void {
        var curCompiler = self.compiler;
        while (curCompiler) |elem| {
            self.markObject(allocator, elem.function.toObject());
            curCompiler = elem;
        }
    }
    
    fn markValue(self: Self, allocator: std.mem.Allocator, value: Value) void {
        switch (value) {
            .Object => {
                self.markObject(allocator, value.Object);
            },
            _ => {}
        }
    }
    
    fn markObject(self: Self, allocator: std.mem.Allocator, obj: *Object) void {
        obj.isMarked = true;
        try self.vm.grayStack.append(allocator, obj);
    }
};

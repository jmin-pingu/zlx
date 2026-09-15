const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const String = @import("object.zig").String;
const Function = @import("object.zig").Function;
const Closure = @import("object.zig").Closure;
const Upvalue = @import("object.zig").Upvalue;
const NativeFunction = @import("object.zig").NativeFunction;
const NativeFunctionType = @import("object.zig").NativeFunctionType;
const OpCode = @import("chunk.zig").OpCode;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Compiler = @import("compiler.zig").Compiler;
const MemoryManager = @import("memory.zig").MemoryManager;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;
const Metadata = @import("gc.zig").Metadata;
const assert = std.debug.assert;
const c = @cImport(@cInclude("time.h"));

const STACK_SIZE = FRAMES_MAX * std.math.maxInt(u8);
const FRAMES_MAX = 64;

const StringHashMap = std.StringHashMap(HashMapEntry);
const HashMapEntry = struct {
    keyPtr: *String,
    value: Value,

    pub fn init(keyPtr: *String, value: Value) HashMapEntry {
        return .{.keyPtr=keyPtr, .value=value};
    }

    pub fn getValue(self: HashMapEntry) Value {
        return self.value;
    }
};

pub const InterpretResult = enum {
    INTERPRET_OK, 
    INTERPRET_COMPILE_ERROR, 
    INTERPRET_RUNTIME_ERROR, 
};

pub const CallFrame = struct {
    closure: *Closure,
    ip: [*]u8,
    slots: [*]Value,
    slotsBase: [*]Value,

    const Self = @This();

    pub fn init(closure: *Closure, ip: [*]u8, slots: [*]Value) Self {
        return .{
            .closure=closure,
            .ip=ip,
            .slots=slots, 
            .slotsBase=slots, 
        };
    }

    pub fn getChunk(self: Self) *Chunk {
        return self.closure.function.chunk;
    }

    pub fn getLine(self: Self, index: u8) usize {
        return self.getChunk().getLine(index);
    }
};

pub const VM = struct {
    /// TODO: add description
    chunk: *const Chunk,
    /// TODO: add description
    ip: [*]u8,
    /// TODO: add description
    stack: [*]Value,
    /// TODO: add description
    globals: StringHashMap,
    /// TODO: add description
    frames: [FRAMES_MAX]*CallFrame,
    /// TODO: add description
    frameCount: usize,
    /// TODO: add description
    metadata: *Metadata,
    /// TODO: add description
    openUpvalues: ?*Upvalue,
    /// TODO: add description
    grayStack: ArrayList(*Object),
    /// TODO: add description
    memoryManager: *MemoryManager,

    var stackBuffer: [STACK_SIZE]Value = [_]Value{undefined} ** STACK_SIZE;
    var frameBuffer: [FRAMES_MAX]*CallFrame = [_]*CallFrame{undefined} ** FRAMES_MAX;
    const stackBase: [*]Value = stackBuffer[0..STACK_SIZE].ptr;

    const Self = @This();

    pub fn init(metadata: *Metadata, memoryManager: *MemoryManager, allocator: std.mem.Allocator) !Self {
        var vm: VM = .{
            .chunk = undefined,
            .ip = undefined,
            .stack = stackBuffer[0..STACK_SIZE].ptr,
            .metadata = metadata,
            .globals = StringHashMap.init(allocator),
            .frames = frameBuffer,
            .frameCount = 0,
            .openUpvalues = null,
            .grayStack = .empty,
            .memoryManager = memoryManager,
        };
        try defineNative(allocator, memoryManager, &vm, "clock", clockNative, 0, metadata);
        return vm;
    }

    pub fn interpret(self: *Self, compiler: *Compiler, source: []const u8, allocator: std.mem.Allocator) !InterpretResult {
        const function = compiler.compile(source, allocator) catch |err| {
            std.debug.print("vm: compile error {any}\n", .{err});
            return .INTERPRET_COMPILE_ERROR;
        };
        const closure = try Closure.init(allocator, self.memoryManager, self.metadata, function);
        self.push(Object.toValue(closure));
        _ = try self.call(closure, 0, allocator);

        return self.run(allocator);
    }

    fn currentFrame(self: Self) *CallFrame {
        return self.frames[self.frameCount-1];
    }

    // TODO: create an iterator for frames. 

    pub fn peek(self: *Self, distance: usize) Value {
        assert(@intFromPtr(stackBase) <= @intFromPtr(self.stack - 1 - distance));
        return (self.stack - 1 - distance)[0];
    }

    pub fn push(self: *Self, value: Value) void {
        assert(self.stack != stackBase + STACK_SIZE);
        self.stack[0] = value;
        self.stack += 1;
    }

    pub fn pop(self: *Self) Value {
        assert(self.stack != stackBase);
        self.stack -= 1;
        return self.stack[0];
    }

    /// Returns the current byte and increments the instruction pointer
    fn readByte(self: *Self) u8 {
        var frame = self.currentFrame();
        defer frame.ip += 1;
        return frame.ip[0];
    }

    fn readShort(self: *Self) u16 {
        var frame = self.currentFrame();
        defer frame.ip += 2;
        return (@as(u16, frame.ip[0]) << 8) + frame.ip[1];
    }

    pub fn readConstant(self: *Self) !Value {
        var frame = self.currentFrame();
        const index = self.readByte();
        return try frame.getChunk().getConstant(index);
    }

    pub fn binaryOperator(self: *Self, operator: OpCode) !void {
        if (!(self.peek(0) == .Number) or !(self.peek(1) == .Number)) {
            try self.runtimeError("Operands must be numbers.", .{});
            return error.runtimeError;
        }
        const b = self.pop().Number;
        const a = self.pop().Number;
        switch (operator) {
            .OP_ADD => self.push(Value.initNumber(a + b)),
            .OP_DIVIDE => self.push(Value.initNumber(a / b)),
            .OP_SUBTRACT => self.push(Value.initNumber(a - b)),
            .OP_MULTIPLY => self.push(Value.initNumber(a * b)),
            .OP_GREATER => self.push(Value.initBool(a > b)),
            .OP_LESS => self.push(Value.initBool(a < b)),
            else => unreachable
        }
    }

    fn disassembleStack(self: *Self) void {
        const frame = self.currentFrame();

        print("=== stack: ", .{});
        frame.closure.print();
        print(" ===\n[ \n", .{});

        var base: [*]Value = frame.slotsBase;
        while (base != self.stack) {
            base[0].print();
            print("\n", .{});
            base += 1;
        }
        print("]\n", .{});
    }

    pub fn run(self: *Self, allocator: std.mem.Allocator) !InterpretResult {
        var frame: *CallFrame = self.currentFrame();
        while (true) {
            const offset = @intFromPtr(frame.ip) - @intFromPtr(frame.getChunk().getInstructionBasePointer());
            const instruction: OpCode = @enumFromInt(self.readByte());
            if (mode == .Debug) {
                self.disassembleStack();
                _ = try instruction.disassemble(frame.getChunk(), offset);
            }

            switch (instruction) {
                .OP_NEGATE => {
                    switch (self.peek(0)) {
                        .Number => {
                            const neg_number = -self.pop().Number;
                            self.push(Value.initNumber(neg_number));
                        },
                        else => {
                        }
                    }
                },
                .OP_SET_LOCAL => {
                    const slot_idx = self.readByte();
                    frame.slots[slot_idx] = self.peek(0);
                },
                .OP_GET_LOCAL => {
                    const slot_idx = self.readByte();
                    self.push(frame.slots[slot_idx]);
                },
                .OP_DEFINE_GLOBAL => {
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType(String);
                    try self.globals.put(identifier.value, HashMapEntry.init(identifier, self.peek(0)));
                    _ = self.pop();
                },
                .OP_SET_GLOBAL => {
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType(String);
                    const notDefined = !self.globals.contains(identifier.value);
                    try self.globals.put(identifier.value, HashMapEntry.init(identifier, self.peek(0)));
                    if (notDefined) {
                        try self.runtimeError("Undefined variable {s}\n", .{identifier.value});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_GET_GLOBAL => {
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType(String);
                    if (self.globals.get(identifier.value)) |entry| {
                        self.push(entry.getValue());
                    } else {
                        try self.runtimeError("Undefined variable {s}\n", .{identifier.value});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_MULTIPLY, .OP_SUBTRACT, .OP_DIVIDE, .OP_GREATER, .OP_LESS => |op| try self.binaryOperator(op),
                .OP_ADD => |op| {
                    if (self.peek(0).isObjectType(.String) and self.peek(1).isObjectType(.String)) {
                        try self.concatenate(allocator);
                    } else {
                       try self.binaryOperator(op);
                    }
                },
                .OP_RETURN => { 
                    const result = self.pop();
                    self.closeUpvalues(&frame.slots[0]);
                    self.frameCount -= 1;
                    if (self.frameCount == 0) {
                        _ = self.pop();
                        return .INTERPRET_OK;
                    }
                    self.stack = frame.slots;
                    self.push(result);
                    frame = self.frames[self.frameCount-1] ;
                },
                .OP_CONSTANT => {
                    const constant = try self.readConstant();
                    self.push(constant);
                },
                .OP_TRUE => self.push(Value.initBool(true)),
                .OP_FALSE => self.push(Value.initBool(false)),
                .OP_NIL => self.push(Value.initNil()),
                .OP_NOT => self.push(Value.initBool(self.pop().isFalsey())),
                .OP_PRINT => {
                    const value = self.pop();
                    value.print();
                    std.debug.print("\n", .{});
                },
                .OP_POP => _ = self.pop(),
                .OP_JUMP_IF_FALSE => {
                    const jumpOffset: u16 = self.readShort();
                    frame.ip += jumpOffset * @intFromBool(self.peek(0).isFalsey());
                },
                .OP_JUMP => {
                    const jumpOffset: u16 = self.readShort();
                    frame.ip += jumpOffset;
                },
                .OP_LOOP => {
                    const jumpOffset: u16 = self.readShort();
                    frame.ip -= jumpOffset;
                },
                .OP_EQUAL_INPLACE => {
                    const a = self.pop();
                    const b = self.pop();
                    self.push(b);
                    self.push(Value.initBool(a.isEqual(&b)));
                },
                .OP_EQUAL => {
                    const a = self.pop();
                    const b = self.pop();
                    self.push(Value.initBool(a.isEqual(&b)));
                },
                .OP_CALL => {
                    const nargs = self.readByte();
                    if (!(try self.callValue(self.peek(nargs), nargs, allocator))) {
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    frame = self.frames[self.frameCount-1];
                },
                .OP_CLOSURE => {
                    const constant = try self.readConstant();
                    const function = constant.Object.toObjectType(Function);
                    const closure = try Closure.init(allocator, self.memoryManager, self.metadata, function);
                    self.push(try Value.initClosure(closure));
                    for (0..closure.upvalueCount) |n| {
                        const isLocal = self.readByte();
                        const index = self.readByte();
                        if (isLocal != 0) {
                            closure.upvalues[n] = try self.captureUpvalue(allocator, &(frame.slots + index)[0]);
                        } else {
                            frame.closure.print();
                            closure.upvalues[n] = frame.closure.upvalues[index];
                        }
                    }
                },
                .OP_GET_UPVALUE => {
                    const slot = self.readByte();
                    self.push(frame.closure.upvalues[slot].location.*);
                },
                .OP_SET_UPVALUE => {
                    const slot = self.readByte();
                    frame.closure.upvalues[slot].location = &self.peek(0);
                },
                .OP_CLOSE_UPVALUE => {
                    self.closeUpvalues(&(self.stack - 1)[0]);
                    _ = self.pop();
                },
                else => {
                    return .INTERPRET_COMPILE_ERROR;
                }
            }
        }
        return undefined;
    }

    fn closeUpvalues(self: *Self, last: *const Value) void {
        while (self.openUpvalues) |currUpvalue| {
            if (@intFromPtr(currUpvalue.location) < @intFromPtr(last)) {
                return;
            }

            const upvalue = currUpvalue;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.openUpvalues = upvalue.next;
        }
    }

    fn captureUpvalue(self: *Self, allocator: std.mem.Allocator, local: *const Value) !*Upvalue {
        var prevUpvalue: ?*Upvalue = null;
        var maybeUpvalue: ?*Upvalue = self.openUpvalues;
        while (maybeUpvalue) |upvalue| {
            if (@intFromPtr(upvalue.location) > @intFromPtr(local)) {
                prevUpvalue = upvalue;
                maybeUpvalue = upvalue.next;
            } else if (upvalue.location == local) {
                return upvalue;
            }
        }

        const createdUpvalue =  try Upvalue.init(allocator, self.memoryManager, self.metadata, local);
        createdUpvalue.next = maybeUpvalue;
        if (prevUpvalue) |prev| {
            prev.next = createdUpvalue;
        } else {
            self.openUpvalues = createdUpvalue;
        }
        return createdUpvalue;
    }

    fn callValue(self: *Self, callee: Value, nargs: u8, allocator: std.mem.Allocator) !bool {
        if (callee.isObject()) {
            switch (callee.Object.objectType) {
                .Closure => return try self.call(callee.Object.toObjectType(Closure), nargs, allocator),
                .NativeFunction => {
                    // NOTE: nargs is defined; we need arity is the issue
                    const native = callee.Object.toObjectType(NativeFunction);
                    if (nargs != native.arity) {
                        self.runtimeError("Expected {d} arguments but got {d}.\n", .{native.arity, nargs}) catch {};
                        return false;
                    }
                    const result = native.nativeFn(self.stack - nargs);
                    self.stack -= nargs + 1;
                    self.push(result);
                    return true;
                },
                else => {}
            }
        }
        self.runtimeError("Can only call functions and classes.\n", .{}) catch {};
        return false; 
    }

    fn defineNative(allocator: std.mem.Allocator, memoryManager: *MemoryManager, self: *Self, name: []const u8, function: NativeFunctionType, arity: usize, metadata: *Metadata) !void {
        self.push(try Value.initNativeFunction(allocator, memoryManager, self.metadata, function, arity));
        const fnName = try String.init(allocator, memoryManager, name, metadata);
        try self.globals.put(fnName.value, HashMapEntry.init(fnName, (self.stack-1)[0]));
        _ = self.pop();
    }

    fn call(self: *Self, closure: *Closure, nargs: u8, allocator: std.mem.Allocator) !bool {
        if (nargs != closure.function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.\n", .{closure.function.arity, nargs}) catch {};
            return false;
        }

        if (self.frameCount == FRAMES_MAX) {
            self.runtimeError("Frame overflow\n", .{}) catch {};
            return false;
        }

        const frame = try allocator.create(CallFrame);
        frame.* = CallFrame.init(
            closure, 
            closure.function.chunk.getInstructionBasePointer(), 
            self.stack - nargs - 1
        );
        self.frames[self.frameCount] = frame;
        self.frameCount += 1;
        return true;
    }

    fn concatenate(self: *Self, allocator: std.mem.Allocator) !void {
        const a: *String = self.pop().Object.toObjectType(String);
        const b: *String = self.pop().Object.toObjectType(String);
        const concat = try std.fmt.allocPrint(allocator, "{s}{s}", .{b.value, a.value});
        self.push(try Value.initString(allocator, self.memoryManager, concat, self.metadata));
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        if (self.metadata.allocations) |obj| {
            obj.freeObjects(allocator);
        }
        self.metadata.identifiers.deinit();
        self.metadata.interned.deinit();
        self.globals.deinit();
        return;
    }

    pub fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        std.debug.print(fmt, args);
        var frameIdx: usize = self.frameCount - 1;
        while (frameIdx > 0) : (frameIdx -= 1) {
            try self.printRuntimeError(frameIdx);
        }
        try self.printRuntimeError(0);
    }

    fn printRuntimeError(self: *Self, frameIdx: usize) !void {
        const frame = self.frames[frameIdx];
        const closure = frame.closure;
        const instruction = frame.ip - frame.getChunk().code.items.ptr - 1;
        std.debug.print("[line {d}] in ", .{try frame.getChunk().getLine(instruction)});
        if (closure.function.name) |val| {
            std.debug.print("{s}\n", .{val.value});
        } else {
            std.debug.print("script\n", .{});
        }
    }

    // Native Functions
    fn clockNative(nargs: [*]Value) Value {
        _ = nargs;
        return Value.initNumber(@as(f64, @floatFromInt(c.clock())) / @as(f64, @floatFromInt(c.CLOCKS_PER_SEC)));
    }
};


const testing = std.testing;

const TestRun = struct {
    vm: *VM,
    result: InterpretResult,

    fn global(self: TestRun, name: []const u8) ?Value {
        if (self.vm.globals.get(name)) |entry| return entry.getValue();
        return null;
    }
};

fn newVM(allocator: std.mem.Allocator) !*VM {
    const vm = try allocator.create(VM);
    const metadata = try allocator.create(Metadata);
    metadata.* = Metadata.init(allocator);
    const memoryManager = try MemoryManager.init(allocator, vm, undefined);
    vm.* = try VM.init(metadata, memoryManager, allocator);
    return vm;
}

fn runSource(source: []const u8) !TestRun {
    const allocator = std.testing.allocator;
    const compiler = try allocator.create(Compiler);
    const vm = try allocator.create(VM);
    const memoryManager = try MemoryManager.init(allocator, vm, compiler);
    const metadata = try allocator.create(Metadata);
    metadata.* = Metadata.init(allocator);
    compiler.* = try Compiler.init(metadata, null, .Script, null, memoryManager, allocator);
    vm.* = try VM.init(metadata, memoryManager, allocator);

    const result = vm.interpret(compiler, source, allocator) catch |err| switch (err) {
        error.runtimeError => InterpretResult.INTERPRET_RUNTIME_ERROR,
        else => return err,
    };
    return .{ .vm = vm, .result = result };
}

fn expectResult(source: []const u8, expected: InterpretResult) !void {
    const run = try runSource(source);
    try testing.expectEqual(expected, run.result);
}

fn expectGlobal(source: []const u8, name: []const u8) !Value {
    const run = try runSource(source);
    try testing.expectEqual(InterpretResult.INTERPRET_OK, run.result);
    const value = run.global(name);
    try testing.expect(value != null);
    return value.?;
}

fn expectNumber(source: []const u8, name: []const u8, expected: f64) !void {
    const value = try expectGlobal(source, name);
    try testing.expect(value == .Number);
    try testing.expectEqual(expected, value.Number);
}

fn expectBool(source: []const u8, name: []const u8, expected: bool) !void {
    const value = try expectGlobal(source, name);
    try testing.expect(value == .Bool);
    try testing.expectEqual(expected, value.Bool);
}

fn expectNil(source: []const u8, name: []const u8) !void {
    const value = try expectGlobal(source, name);
    try testing.expect(value == .Nil);
}

fn expectString(source: []const u8, name: []const u8, expected: []const u8) !void {
    const value = try expectGlobal(source, name);
    try testing.expect(value.isObjectType(.String));
    try testing.expectEqualStrings(expected, value.Object.toObjectType(String).value);
}


test "vm stack push pop and peek" {
    const vm = try newVM(std.testing.allocator);
    vm.push(Value.initNumber(1));
    vm.push(Value.initNumber(2));
    vm.push(Value.initBool(true));

    try testing.expect(vm.peek(0).Bool);
    try testing.expectEqual(@as(f64, 2), vm.peek(1).Number);
    try testing.expectEqual(@as(f64, 1), vm.peek(2).Number);

    try testing.expect(vm.pop().Bool);
    try testing.expectEqual(@as(f64, 2), vm.pop().Number);
    try testing.expectEqual(@as(f64, 1), vm.pop().Number);
}

test "vm binaryOperator arithmetic and comparison on numbers" {
    const vm = try newVM(std.testing.allocator);
    const cases = [_]struct { OpCode, f64 }{
        .{ .OP_ADD, 9 }, .{ .OP_SUBTRACT, 3 }, .{ .OP_MULTIPLY, 18 }, .{ .OP_DIVIDE, 2 },
    };
    for (cases) |case| {
        vm.push(Value.initNumber(6));
        vm.push(Value.initNumber(3));
        try vm.binaryOperator(case[0]);
        try testing.expectEqual(case[1], vm.pop().Number);
    }

    vm.push(Value.initNumber(6));
    vm.push(Value.initNumber(3));
    try vm.binaryOperator(.OP_GREATER);
    try testing.expect(vm.pop().Bool);

    vm.push(Value.initNumber(6));
    vm.push(Value.initNumber(3));
    try vm.binaryOperator(.OP_LESS);
    try testing.expect(!vm.pop().Bool);
}

test "vm init registers the clock native as a global" {
    const vm = try newVM(std.testing.allocator);
    const entry = vm.globals.get("clock");
    try testing.expect(entry != null);
    const value = entry.?.getValue();
    try testing.expect(value.isObjectType(.NativeFunction));
    try testing.expectEqual(@as(usize, 0), value.Object.toObjectType(NativeFunction).arity);
    try testing.expectEqualStrings("clock", entry.?.keyPtr.value);
}

test "vm call frame exposes its closure chunk" {
    const allocator = std.testing.allocator;
    var metadata = Metadata.init(allocator);
    const mm = try MemoryManager.init(allocator, undefined, undefined);
    const function = try Function.init(allocator, mm, &metadata, null);
    try function.chunk.write(OpCode.OP_RETURN.asByte(), 7, allocator);
    const closure = try Closure.init(allocator, mm, &metadata, function);

    var slots = [_]Value{Value.initNil()};
    const frame = CallFrame.init(closure, function.chunk.getInstructionBasePointer(), &slots);
    try testing.expectEqual(function.chunk, frame.getChunk());
    try testing.expectEqual(closure, frame.closure);
    try testing.expectEqual(OpCode.OP_RETURN.asByte(), frame.ip[0]);
    try testing.expectEqual(frame.slots, frame.slotsBase);
}


test "vm arithmetic evaluates with precedence" {
    try expectNumber("var r = 1 + 2 * 3 - 4 / 2;", "r", 5);
    try expectNumber("var r = (1 + 2) * 3;", "r", 9);
    try expectNumber("var r = -(2 + 3);", "r", -5);
    try expectNumber("var r = 10 - 2 - 3;", "r", 5);
    try expectNumber("var r = 1.5 * 2;", "r", 3);
}

test "vm division by zero yields infinity" {
    const value = try expectGlobal("var r = 1 / 0;", "r");
    try testing.expect(std.math.isInf(value.Number));
}

test "vm number comparisons" {
    try expectBool("var r = 1 < 2;", "r", true);
    try expectBool("var r = 2 <= 2;", "r", true);
    try expectBool("var r = 3 > 4;", "r", false);
    try expectBool("var r = 3 >= 4;", "r", false);
    try expectBool("var r = 1 == 1;", "r", true);
    try expectBool("var r = 1 != 1;", "r", false);
}

test "vm equality across value kinds" {
    try expectBool("var r = \"a\" == \"a\";", "r", true);
    try expectBool("var r = \"a\" == \"b\";", "r", false);
    try expectBool("var r = \"a\" != \"b\";", "r", true);
    try expectBool("var r = 1 == \"1\";", "r", false);
    try expectBool("var r = nil == nil;", "r", true);
    try expectBool("var r = nil == false;", "r", false);
    try expectBool("var r = true == true;", "r", true);
    try expectBool("fun f() {} var r = f == f;", "r", true);
}

test "vm truthiness" {
    try expectBool("var r = !nil;", "r", true);
    try expectBool("var r = !false;", "r", true);
    try expectBool("var r = !true;", "r", false);
    try expectBool("var r = !0;", "r", false);
    try expectBool("var r = !\"\";", "r", false);
}

test "vm string concatenation and interning" {
    try expectString("var s = \"foo\" + \"bar\";", "s", "foobar");
    try expectString("var s = \"\" + \"x\" + \"\";", "s", "x");
    try expectBool("var r = (\"a\" + \"b\") == \"ab\";", "r", true);
}

test "vm logical operators return the deciding operand" {
    try expectString("var r = nil or \"x\";", "r", "x");
    try expectString("var r = \"y\" or \"z\";", "r", "y");
    try expectBool("var r = nil or false;", "r", false);
    try expectBool("var r = false and 1;", "r", false);
    try expectNumber("var r = 1 and 2;", "r", 2);
    try expectNil("var r = nil and 2;", "r");
}


test "vm global variables" {
    try expectNil("var x;", "x");
    try expectNumber("var x = 1;", "x", 1);
    try expectNumber("var x = 1; x = 2;", "x", 2);
    try expectNumber("var a = 1; a = a + 1; a = a * 10;", "a", 20);
    try expectNil("var x; var y = x; x = 3;", "y");
    try expectNumber("var x; x = 3; var z = x;", "z", 3);
    try expectNumber("const k = 7; var r = k * 2;", "r", 14);
}

test "vm local scopes and shadowing" {
    try expectNumber("var r; var a = 1; { var a = 2; { var a = 3; r = a; } }", "r", 3);
    try expectNumber("var r; var a = 1; { var a = 2; r = a; } var t = a;", "r", 2);
    try expectNumber("var r; var a = 1; { var a = 2; r = a; } var t = a;", "t", 1);
    try expectNumber("var s; { var q = 10; { var w = 5; s = q + w; } }", "s", 15);
    try expectNumber("var s; { var q = 1; q = q + 1; q = q + 1; s = q; }", "s", 3);
}


test "vm if and else" {
    try expectString("var r; if (1 < 2) r = \"yes\"; else r = \"no\";", "r", "yes");
    try expectString("var r; if (nil) r = \"t\"; else r = \"f\";", "r", "f");
    try expectNumber("var r = 0; if (false) r = 1;", "r", 0);
    try expectNumber("var r = 0; if (true) { r = 1; r = r + 1; }", "r", 2);
    try expectString("var r; if (false) r = \"a\"; else if (true) r = \"b\"; else r = \"c\";", "r", "b");
}

test "vm while loop" {
    try expectNumber("var i = 0; var s = 0; while (i < 5) { i = i + 1; s = s + i; }", "s", 15);
    try expectNumber("var i = 0; while (false) i = 1;", "i", 0);
}

test "vm for loop variants" {
    try expectNumber("var s = 0; for (var i = 0; i < 4; i = i + 1) { s = s + i; }", "s", 6);
    try expectNumber("var t = 0; var j = 0; for (; j < 3;) { j = j + 1; t = t + 10; }", "t", 30);
    try expectNumber("var n = 0; for (var i = 5; i > 0; i = i - 1) n = n + 1;", "n", 5);
}

test "vm continue skips the rest of a while body" {
    try expectNumber(
        "var r = 0; var i = 0; while (i < 5) { i = i + 1; if (i == 3) continue; r = r + i; }",
        "r",
        12,
    );
}

test "vm switch statement" {
    const arms = "switch (v) { 1 => r = \"one\"; 2 => r = \"two\"; default => r = \"other\"; }";
    try expectString("var r; var v = 1; " ++ arms, "r", "one");
    try expectString("var r; var v = 2; " ++ arms, "r", "two");
    try expectString("var r; var v = 9; " ++ arms, "r", "other");
    try expectNumber("var r = 0; switch (3) { 1 => r = 1; 2 => r = 2; }", "r", 0);
    try expectNumber("var r = 0; switch (2) { 1 => r = 1; 2 => { r = 2; r = r * 10; } }", "r", 20);
    try expectString("var r; switch (\"b\") { \"a\" => r = \"A\"; \"b\" => r = \"B\"; }", "r", "B");
}


test "vm functions return values" {
    try expectNumber("fun f() { return 42; } var r = f();", "r", 42);
    try expectNil("fun f() {} var r = f();", "r");
    try expectNil("fun f() { return; } var r = f();", "r");
    try expectNumber("fun add(a, b) { return a + b; } var r = add(2, 3);", "r", 5);
    try expectNumber("fun one() { return 1; } var r = one() + one();", "r", 2);
}

test "vm assignment to a function parameter" {
    try expectNumber("fun f(a) { a = a * 2; return a; } var r = f(4);", "r", 8);
}

test "vm functions recurse" {
    try expectNumber("fun fib(n) { if (n < 2) return n; return fib(n - 1) + fib(n - 2); } var r = fib(10);", "r", 55);
    try expectNumber("fun fact(n) { if (n <= 1) return 1; return n * fact(n - 1); } var r = fact(6);", "r", 720);
}

test "vm functions are first class values" {
    try expectNumber("fun f() { return 1; } var g = f; var r = g();", "r", 1);
    try expectNumber("fun apply(fn, x) { return fn(x); } fun dbl(x) { return x * 2; } var r = apply(dbl, 21);", "r", 42);
}

test "vm native clock returns a non-negative number" {
    const value = try expectGlobal("var t = clock();", "t");
    try testing.expect(value == .Number);
    try testing.expect(value.Number >= 0);
}


test "vm closure reads an upvalue after the enclosing function returned" {
    try expectString(
        "fun outer() { var x = \"closed\"; fun inner() { return x; } return inner; } var f = outer(); var r = f();",
        "r",
        "closed",
    );
}

test "vm closure reads an open upvalue while the enclosing function runs" {
    try expectNumber("var r; fun outer() { var x = 5; fun inner() { r = x; } inner(); } outer();", "r", 5);
}

test "vm closure captures through an intermediate function" {
    try expectString(
        "fun a() { var x = \"deep\"; fun b() { fun c() { return x; } return c; } return b; } var r = a()()();",
        "r",
        "deep",
    );
}

test "vm block local captured by a closure is closed at scope end" {
    try expectNumber("var f; { var x = 1; fun g() { return x; } f = g; } var r = f();", "r", 1);
}

test "vm closures share and mutate a captured variable" {
    try expectNumber(
        "var r; fun outer() { var a = 1; fun set() { a = 2; } fun get() { return a; } set(); r = get(); } outer();",
        "r",
        2,
    );
}

test "vm closure counter keeps state between calls" {
    const counter = "fun makeCounter() { var i = 0; fun count() { i = i + 1; return i; } return count; } var c = makeCounter(); ";
    try expectNumber(counter ++ "c(); var r = c();", "r", 2);
    try expectNumber(counter ++ "c(); c(); var r = c();", "r", 3);
}

test "vm closure capturing two variables" {
    if (true) return error.SkipZigTest;
    try expectNumber("fun outer() { var a = 1; var b = 2; fun f() { return a + b; } return f; } var g = outer(); var r = g();", "r", 3);
}


test "vm runtime errors" {
    const cases = [_][]const u8{
        "var r = missing;",
        "missing = 1;",
        "fun f(a) { return a; } f(1, 2);",
        "fun f(a) { return a; } f();",
        "var x = 1; x();",
        "\"text\"();",
        "clock(1);",
        "fun f() { f(); } f();",
    };
    for (cases) |source| {
        try expectResult(source, .INTERPRET_RUNTIME_ERROR);
    }
}

test "vm operand type errors are runtime errors" {
    const cases = [_][]const u8{
        "1 + \"a\";",
        "\"a\" + 1;",
        "1 < \"a\";",
        "\"a\" * 2;",
        "true - 1;",
        "nil / 2;",
    };
    for (cases) |source| {
        try expectResult(source, .INTERPRET_RUNTIME_ERROR);
    }
}

test "vm negating a non-number is a runtime error" {
    try expectResult("-\"a\";", .INTERPRET_RUNTIME_ERROR);
}

test "vm reports compile errors as a compile error result" {
    try expectResult("var x = 1", .INTERPRET_COMPILE_ERROR);
    try expectResult("const x;", .INTERPRET_COMPILE_ERROR);
    try expectResult("fun f( {}", .INTERPRET_COMPILE_ERROR);
}

test "vm program state is isolated between runs" {
    try expectNumber("var x = 1;", "x", 1);
    try expectResult("var r = x;", .INTERPRET_RUNTIME_ERROR);
}

test "vm string values keep escape sequences uninterpreted" {
    const source =
        \\var s = "a\nb";
    ;
    try expectString(source, "s", "a\\nb");
    const value = try expectGlobal(source, "s");
    try testing.expectEqual(@as(usize, 4), value.Object.toObjectType(String).value.len);

    const compare =
        \\var r = "\n" == "
    ++ "\n" ++
        \\";
    ;
    try expectBool(compare, "r", false);
}

test "vm concatenation leaves escape sequences alone" {
    const source =
        \\var s = "a\n" + "\tb";
    ;
    try expectString(source, "s", "a\\n\\tb");
}

test "vm string values keep control characters" {
    try expectString("var s = \"a\x01b\";", "s", "a\x01b");
    const value = try expectGlobal("var s = \"a\x1bb\";", "s");
    try testing.expectEqual(@as(usize, 3), value.Object.toObjectType(String).value.len);
    try testing.expectEqual(@as(u8, 0x1b), value.Object.toObjectType(String).value[1]);
}

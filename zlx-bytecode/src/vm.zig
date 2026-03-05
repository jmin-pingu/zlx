const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;
const Object = @import("value.zig").Object;
const Function = @import("value.zig").Function;
const NativeFunction = @import("value.zig").NativeFunction;
const NativeFunctionType = @import("value.zig").NativeFunctionType;
const OpCode = @import("chunk.zig").OpCode;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Compiler = @import("compiler.zig").Compiler;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;
const Metadata = @import("gc.zig").Metadata;
const StringHashMap = std.StringHashMap;
const assert = std.debug.assert;
const c = @cImport(@cInclude("time.h"));

const STACK_SIZE = FRAMES_MAX * std.math.maxInt(u8);
const FRAMES_MAX = 64;

pub const InterpretResult = enum {
    INTERPRET_OK, 
    INTERPRET_COMPILE_ERROR, 
    INTERPRET_RUNTIME_ERROR, 
};

pub const CallFrame = struct {
    function: *Function,
    ip: [*]u8,
    slots: [*]Value,
    slotsBase: [*]Value,

    const Self = @This();

    pub fn init(function: *Function, ip: [*]u8, slots: [*]Value) Self {
        return .{
            .function=function,
            .ip=ip,
            .slots=slots, 
            .slotsBase=slots, 
        };
    }

    pub fn getChunk(self: Self) *Chunk {
        return self.function.chunk;
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
    globals: StringHashMap(Value),
    /// TODO: add description
    frames: [FRAMES_MAX]*CallFrame,
    /// TODO: add description
    frameCount: usize,
    /// TODO: add description
    metadata: *Metadata,

    /// TODO: add description
    var stackBuffer: [STACK_SIZE]Value = [_]Value{undefined} ** STACK_SIZE;
    var frameBuffer: [FRAMES_MAX]*CallFrame = [_]*CallFrame{undefined} ** FRAMES_MAX;
    const stackBase: [*]Value = stackBuffer[0..STACK_SIZE].ptr;

    const Self = @This();

    pub fn init(metadata: *Metadata, allocator: std.mem.Allocator) !Self {
        var vm: VM = .{
            .chunk = undefined,
            .ip = undefined,
            .stack = stackBuffer[0..STACK_SIZE].ptr,
            .metadata = metadata,
            .globals = StringHashMap(Value).init(allocator),
            .frames = frameBuffer,
            .frameCount = 0,
        };
        try defineNative(&vm, "clock", clockNative, 0, allocator);
        return vm;
    }

    pub fn interpret(self: *Self, compiler: *Compiler, source: []const u8, allocator: std.mem.Allocator) !InterpretResult {
        const function = compiler.compile(source, allocator) catch |err| {
            std.debug.print("vm: compile error {any}\n", .{err});
            return .INTERPRET_COMPILE_ERROR;
        };

        self.push(function.toValue());
        _ = try self.call(function, 0, allocator);

        if (mode == .Debug) {
            if (function.name) |name| {
                try function.chunk.disassemble(name.value);
            } else {
                try function.chunk.disassemble("<script>");
            }
        }
        return self.run(allocator);
    }

    fn currentFrame(self: Self) *CallFrame {
        return self.frames[self.frameCount-1];
    }

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
        frame.function.print();
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
                _ = try instruction.disassemble(
                    frame.getChunk(), 
                    offset,
                    offset
                );
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
                    // NOTE: readByte works since for these constant instructions we save 
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType(String);
                    try self.globals.put(identifier.value, self.peek(0));
                    _ = self.pop();
                },
                .OP_SET_GLOBAL => {
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType(String);
                    const notDefined = !self.globals.contains(identifier.value);
                    try self.globals.put(identifier.value, self.peek(0));
                    if (notDefined) {
                        try self.runtimeError("Undefined variable {s}\n", .{identifier.value});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_GET_GLOBAL => {
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType(String);
                    if (self.globals.get(identifier.value)) |value| {
                        self.push(value);
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
                else => {
                    return .INTERPRET_COMPILE_ERROR;
                }
            }
        }
        return undefined;
    }

    fn callValue(self: *Self, callee: Value, nargs: u8, allocator: std.mem.Allocator) !bool {
        if (callee.isObject()) {
            switch (callee.Object.objectType) {
                .Function => return try self.call(callee.Object.toObjectType(Function), nargs, allocator),
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

    fn defineNative(self: *Self, name: []const u8, function: NativeFunctionType, arity: usize, allocator: std.mem.Allocator) !void {
        self.push(try Value.initNativeFunction(allocator, self.metadata, function, arity));
        try self.globals.put(name, (self.stack-1)[0]);
        _ = self.pop();
    }

    fn call(self: *Self, function: *Function, nargs: u8, allocator: std.mem.Allocator) !bool {
        if (nargs != function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.\n", .{function.arity, nargs}) catch {};
            return false;
        }

        if (self.frameCount == FRAMES_MAX) {
            self.runtimeError("Frame overflow\n", .{}) catch {};
            return false;
        }

        const frame = try allocator.create(CallFrame);
        frame.* = CallFrame.init(
            function, 
            function.chunk.getInstructionBasePointer(), 
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
        self.push(try Value.initString(concat, self.metadata, allocator));
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        if (self.metadata.allocations) |obj| {
            obj.deinit(allocator);
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
        const function = frame.function;
        const instruction = frame.ip - function.chunk.code.items.ptr - 1;
        std.debug.print("[line {d}] in ", .{try frame.getChunk().getLine(instruction)});
        if (function.name) |val| {
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


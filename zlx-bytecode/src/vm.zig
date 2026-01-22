const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;
const Object = @import("value.zig").Object;
const Function = @import("value.zig").Function;
const OpCode = @import("chunk.zig").OpCode;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Compiler = @import("compiler.zig").Compiler;
const debug = @import("error.zig").debug;
const mode = @import("main.zig").mode;
const Metadata = @import("gc.zig").Metadata;
const StringHashMap = std.StringHashMap;
const assert = std.debug.assert;

pub const InterpretResult = enum {
    INTERPRET_OK, 
    INTERPRET_COMPILE_ERROR, 
    INTERPRET_RUNTIME_ERROR, 
};

const stack_size = frames_max * std.math.maxInt(u8);
const frames_max = 64;

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
        return self.getChunk().line.decode(index);
    }
};

pub const VM = struct {
    chunk: *const Chunk,
    ip: [*]u8,
    stack: [*]Value,
    globals: StringHashMap(Value),
    frames: [frames_max]*CallFrame,
    frameCount: usize,
    metadata: *Metadata,
    var stackBuffer: [stack_size]Value = [_]Value{undefined} ** stack_size;
    var frameBuffer: [frames_max]*CallFrame = [_]*CallFrame{undefined} ** frames_max;
    const stackBase: [*]Value = stackBuffer[0..stack_size].ptr;

    const Self = @This();

    pub fn init(metadata: *Metadata, allocator: std.mem.Allocator) !Self {
        return .{
            .chunk = undefined,
            .ip = undefined,
            .stack = stackBuffer[0..stack_size].ptr,
            .metadata = metadata,
            .globals = StringHashMap(Value).init(allocator),
            .frames = frameBuffer,
            .frameCount = 0,
        };
    }

    pub fn interpret(self: *Self, compiler: *Compiler, source: []const u8, allocator: std.mem.Allocator) !InterpretResult {
        const function = compiler.compile(source, allocator) catch |err| {
            std.debug.print("vm: compile error {any}\n", .{err});
            return .INTERPRET_COMPILE_ERROR;
        };
        // NOTE: this is the main part that I'm suspect about
        self.stack[0] = function.toValue();
        self.stack += 1;
        const frame = try allocator.create(CallFrame);
        frame.* = CallFrame.init(
            function, 
            function.chunk.getInstructionBasePointer(), 
            self.stack
        );

        self.frames[self.frameCount] = frame;
        self.frameCount += 1;

        if (mode == .Debug) {
            try function.chunk.disassemble("current chunk");
        }
        std.debug.print("{any}\n", .{frame.getChunk().code});

        return self.run(allocator);
    }

    fn currentFrame(self: Self) *CallFrame {
        return self.frames[self.frameCount-1];
    }

    pub fn peek(self: *Self, distance: usize) Value {
        const frame = self.currentFrame();
        const stackPos: [*]Value = frame.slots - distance - 1;
        assert(!(@intFromPtr(frame.slotsBase) > @intFromPtr(stackPos)));
        return stackPos[0];
    }

    pub fn push(self: *Self, value: Value) void {
        // NOTE: need to know upper limit of frames
        var frame = self.currentFrame();
        assert(!(frame.slots == stackBase + stack_size));
        frame.slots[0] = value;
        frame.slots += 1;
    }

    pub fn pop(self: *Self) Value {
        var frame = self.currentFrame();
        assert(frame.slots != frame.slotsBase);
        frame.slots -= 1;
        return frame.slots[0];
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
        print("--- STACK ---\n[ \n", .{});
        var stack_base: [*]Value = frame.slotsBase;
        while (stack_base != frame.slots) {
            print("  ", .{});
            stack_base[0].print();
            print("\n", .{});
            stack_base += 1;
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
                    return .INTERPRET_OK;
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
                else => {}
            }
        }
        // TODO: return error here
        return false; 
    }

    fn call(self: *Self, function: *Function, nargs: u8, allocator: std.mem.Allocator) !bool {
        // TODO: these errors should be fixed!
        if (nargs != function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.\n", .{function.arity, nargs}) catch {};
            return false;
        }

        if (self.frameCount == frames_max) {
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
            const frame = self.frames[frameIdx];
            const function = frame.function;
            const instruction = frame.ip - function.chunk.code.items.ptr - 1;
            std.debug.print("[line {d}] in ", .{try frame.getChunk().line.decode(instruction)});
            if (function.name) |val| {
                std.debug.print("{s}\n", .{val.value});
            } else {
                std.debug.print("script\n", .{});
            }
    // CallFrame* frame = &vm.frames[i];
    // ObjFunction* function = frame->function;
    // size_t instruction = frame->ip - function->chunk.code - 1;
    // fprintf(stderr, "[line %d] in ", 
    //         function->chunk.lines[instruction]);
    // if (function->name == NULL) {
    //   fprintf(stderr, "script\n");
    // } else {
    //   fprintf(stderr, "%s()\n", function->name->chars);
    // }
        }
        // const frame = self.currentFrame();
        // const chunkBase = frame.getChunk().getInstructionBasePointer();
        // if (frame.ip - chunkBase < 0) {
        //     return error.OutOfBoundsError;
        // }
        // // TODO: 
        // const instruction = (frame.ip - 1)[0];
        // std.debug.print("\n[line {d}] in script\n", .{try frame.getChunk().line.decode(instruction)});
    }
};

test "stack" {
    var vm = VM.init();
    vm.push(Value.initNumber(1));
    vm.push(Value.initNumber(2));
    vm.push(Value.initBool(true));
    vm.push(Value.initNumber(4));
    vm.push(Value.initNumber(5));
    std.debug.print("{b}\n", .{Value.initNumber(1) == Value.initNumber(1)});
    std.debug.print("{any}\n", .{vm.peek(3)});
    std.debug.print("{any}\n", .{vm.peek(4)});
    std.debug.print("{any}\n", .{vm.peek(5)});
    try vm.runtimeError("test", .{});
}

const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;
const Object = @import("value.zig").Object;
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

const stack_size = 256;

pub const VM = struct {
    chunk: *const Chunk,
    ip: [*]u8,
    stack: [*]Value,
    stackTop: [*]Value,
    globals: StringHashMap(Value),
    metadata: *Metadata,
    var stackBuffer: [stack_size]Value = [_]Value{undefined} ** stack_size;
    const stackBase: [*]Value = stackBuffer[0..stack_size].ptr;

    const Self = @This();

    pub fn init(metadata: *Metadata, allocator: std.mem.Allocator) Self {
        return .{
            .chunk = undefined,
            .ip = undefined,
            .stackTop = stackBuffer[0..stack_size].ptr,
            .stack = stackBuffer[0..stack_size].ptr,
            .metadata = metadata,
            .globals = StringHashMap(Value).init(allocator)
        };
    }

    pub fn interpret(self: *Self, compiler: *Compiler, source: []const u8, allocator: std.mem.Allocator) !InterpretResult {
        var chunk = Chunk.init(allocator);
        defer chunk.deinit(allocator);
        const result = compiler.compile(source, &chunk, allocator) catch |err| {
            std.debug.print("vm: compile error {any}\n", .{err});
            return .INTERPRET_COMPILE_ERROR;
        };
        if (!result) {
            return .INTERPRET_COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = chunk.code.items[0..chunk.code.items.len].ptr;
        if (mode == .Debug) {
            try self.chunk.disassemble("current chunk");
        }
        return self.run(allocator);
    }

    pub fn peek(self: *Self, distance: usize) Value {
        const stackPos: [*]Value = self.stackTop - distance - 1;
        assert(!(@intFromPtr(stackBase) > @intFromPtr(stackPos)));
        return stackPos[0];
    }

    pub fn push(self: *Self, value: Value) void {
        assert(!(self.stackTop == self.stack + stack_size));
        self.stackTop[0] = value;
        self.stackTop += 1;
    }

    pub fn pop(self: *Self) Value {
        assert(self.stackTop != self.stack);
        self.stackTop -= 1;
        return self.stackTop[0];
    }

    /// Returns the current byte and increments the instruction pointer
    fn readByte(self: *Self) u8 {
        defer self.ip += 1;
        return self.ip[0];
    }

    fn readShort(self: *Self) u16 {
        defer self.ip += 2;
        return (@as(u16, self.ip[0]) << 8) + self.ip[1];
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
        print("stack: [ ", .{});
        var stack_base: [*]Value = self.stack;
        while (stack_base != self.stackTop) {
            print(" {any} ", .{stack_base[0]});
            stack_base += 1;
        }
        print(" ]\n", .{});
    }

    pub fn run(self: *Self, allocator: std.mem.Allocator) !InterpretResult {
        while (true) {
            const offset = @intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr);
            const instruction: OpCode = @enumFromInt(self.readByte());
            if (mode == .Debug) {
                self.disassembleStack();
                _ = try instruction.disassemble(
                    self.chunk, 
                    
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
                    self.stack[slot_idx] = self.peek(0);
                },
                .OP_GET_LOCAL => {
                    const slot_idx = self.readByte();
                    self.push(self.stack[slot_idx]);
                },
                .OP_DEFINE_GLOBAL => {
                    // NOTE: readByte works since for these constant instructions we save 
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType();
                    try self.globals.put(identifier.value, self.peek(0));
                    _ = self.pop();
                },
                .OP_SET_GLOBAL => {
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType();
                    self.globals.put(identifier.value, self.peek(0)) catch {
                        try self.runtimeError("Undefined variable {s}\n", .{identifier.value});
                        return .INTERPRET_RUNTIME_ERROR;
                    };
                },
                .OP_GET_GLOBAL => {
                    const constant = try self.readConstant();
                    const identifier = constant.Object.toObjectType();
                    if (self.globals.get(identifier.value)) |value| {
                        self.push(value);
                    } else {
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
                    self.ip += jumpOffset * @intFromBool(self.peek(0).isFalsey());
                },
                .OP_JUMP => {
                    const jumpOffset: u16 = self.readShort();
                    self.ip += jumpOffset;
                },
                .OP_LOOP => {
                    const jumpOffset: u16 = self.readShort();
                    self.ip -= jumpOffset;
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
                else => {
                    return .INTERPRET_COMPILE_ERROR;
                }
            }
        }
        return undefined;
    }

    pub fn readConstant(self: *Self) !Value {
        const index = self.readByte();
        return try self.chunk.constants.get(index);
    }

    fn concatenate(self: *Self, allocator: std.mem.Allocator) !void {
        const a: *String = self.pop().Object.toObjectType();
        const b: *String = self.pop().Object.toObjectType();
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
        const chunkBase = self.chunk.code.items[0..].ptr;
        if (self.ip - chunkBase < 0) {
            return error.OutOfBoundsError;
        }
        std.debug.print(fmt, args);
        const instruction = (self.ip - 1)[0];
        std.debug.print("\n[line {d}] in script\n", .{try self.chunk.line.decode(instruction)});
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

const std = @import("std");
const Error = @import("error.zig").Error;
const ArrayList = std.ArrayList;
const Value = @import("value.zig").Value;
const RLE = @import("rle.zig").RLE;
const print = std.debug.print;
const Object = @import("value.zig").Object;

pub const OpCode = enum(u8) {
    OP_RETURN,
    OP_CONSTANT,
    OP_CONSTANT_LONG,
    // Unary Operators
    OP_NEGATE,
    OP_NOT,
    // Binary Operators
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    // Literal Operators
    OP_TRUE,
    OP_FALSE,
    OP_NIL,
    // Comparison Operators
    OP_EQUAL,
    OP_EQUAL_INPLACE, // Keeps the first elemtn on th stack but adds an element to the stack for equality
    OP_GREATER,
    OP_LESS,
    // Function Operators
    OP_CALL,
    // Statement Operators
    OP_PRINT,
    OP_POP,
    OP_JUMP_IF_FALSE,
    OP_JUMP,
    OP_LOOP,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,    
    _,

    pub fn asByte(self: OpCode) u8 {
        return @intFromEnum(self);
    }

    pub fn disassemble(self: OpCode, chunk: *const Chunk, offset: usize, rleIdx: usize) !usize {
        print("0x{x:0>4} ", .{offset});

        if (try chunk.line.decodeFirst(rleIdx)) |line| {
            print(" {d:>4} ", .{ line });
        } else {
            print("    | ", .{});
        }

        switch (self) {
            .OP_RETURN, .OP_NEGATE, .OP_ADD, .OP_SUBTRACT, .OP_MULTIPLY, .OP_DIVIDE, .OP_POP,
            .OP_TRUE, .OP_FALSE, .OP_NIL, .OP_NOT, .OP_EQUAL_INPLACE, .OP_EQUAL, .OP_GREATER, .OP_LESS, .OP_PRINT, .OP_CALL => return self.simpleInstruction(offset),
            .OP_CONSTANT, .OP_SET_GLOBAL, .OP_GET_GLOBAL, .OP_DEFINE_GLOBAL => return self.constantInstruction(chunk, offset),
            .OP_SET_LOCAL, .OP_GET_LOCAL => return self.byteInstruction(chunk, offset),
            .OP_CONSTANT_LONG => return self.constantInstructionLong(chunk, offset),
            .OP_JUMP, .OP_JUMP_IF_FALSE => return self.jumpInstruction(chunk, 1, offset),
            .OP_LOOP => return self.jumpInstruction(chunk, -1, offset),
            else => |opCode| {
                print("Unknown opcode {any}\n", .{ opCode });
                return offset + 1;
            },
        }
    }

    fn jumpInstruction(self: OpCode, chunk: *const Chunk, sign: i64, offset: usize) usize {
        var jump = @as(u16, chunk.code.items[offset + 1]) << 8;
        jump |= chunk.code.items[offset + 2];
        print("{s} ip@0x{x:0>4}->0x{x:0>4}\n", .{ 
            @tagName(self), 
            offset, 
            @as(usize, @intCast(@as(i64, @intCast(offset)) + 3 + sign * @as(i64,jump)))
        });
        return offset + 3;
    }

    fn byteInstruction(self: OpCode, chunk: *const Chunk, offset: usize) usize {
        const slot = chunk.code.items[offset+1];
        print("{s} {d}\n", .{ @tagName(self), slot});
        return offset + 2;
    }

    fn simpleInstruction(self: OpCode, offset: usize) usize {
        print("{s}\n", .{ @tagName(self)});
        return offset + 1;
    }

    fn constantInstruction(self: OpCode, chunk: *const Chunk, offset: usize) usize {
        const constantIndex: u8 = chunk.code.items[offset+1];
        const constant = chunk.getConstant(constantIndex) catch unreachable;
        print("{s} {d} `", .{ @tagName(self), constantIndex});
        constant.print();
        print("`\n", .{});
        return offset + 2;
    }

    fn constantInstructionLong(self: OpCode, chunk: *const Chunk, offset: usize) usize {
        const constantIndexSlice: []u8 = chunk.code.items[offset+1..offset+4];
        const index_ptr: *align(1) u24 = std.mem.bytesAsValue(u24, constantIndexSlice);
        const index: usize = @as(usize, index_ptr.*);
        print("{s: <20} {d: <3} `{any}`\n", .{ @tagName(self), index, chunk.getConstant(index) catch unreachable });
        return offset + 4;
    }
};

pub const ValueArray = struct {
    values: ArrayList(Value),

    pub fn init() ValueArray {
        return ValueArray{ .values = .empty };
    }

    pub fn write(self: *ValueArray, byte: u8, allocator: std.mem.Allocator) !void {
        try self.values.append(allocator, byte);
    }

    pub fn get(self: ValueArray, index: usize) !Value {
        if (index >= self.values.items.len) {
            print("{d}\n", .{index});
            return Error.OutOfIndex;
        }
        return self.values.items[index];
    }

    pub fn deinit(self: *ValueArray, allocator: std.mem.Allocator) void {
        self.values.deinit(allocator);
    }
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ValueArray,
    stringConstantsTracker: std.AutoHashMap(*Object, u8),
    line: RLE,

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk { 
            .code = .empty,
            .line = RLE.init(),
            .stringConstantsTracker= std.AutoHashMap(*Object, u8).init(allocator),
            .constants = ValueArray.init(),
        };
    }

    pub fn getInstructionBasePointer(self: *Chunk) [*]u8 {
        return self.code.items.ptr;
    }

    pub fn indexOfLatestInstruction(self: *Chunk) usize {
        return self.code.items.len - 1;
    }

    pub fn indexOfNextInstruction(self: *Chunk) usize {
        return self.code.items.len;
    }

    pub fn getConstant(self: *const Chunk, index: usize) !Value {
        return self.constants.get(index);
    }

    pub fn addConstant(self: *Chunk, value: Value, allocator: std.mem.Allocator) !u8 {
        if (value == .Object and self.stringConstantsTracker.contains(value.Object)) {
            return self.stringConstantsTracker.get(value.Object).?;
        }
        try self.constants.values.append(allocator, value);
        const constantsLength: u8 = @intCast(self.constants.values.items.len);
        if (value == .Object) try self.stringConstantsTracker.put(value.Object, constantsLength - 1);

        return constantsLength - 1;
    }

    pub fn write(self: *Chunk, byte: u8, line: usize, allocator: std.mem.Allocator) !void {
        try self.line.encode(allocator, line);
        try self.code.append(allocator, byte);
    }

    pub fn writeConstantLong(self: *Chunk, constant: Value, line: usize, allocator: std.mem.Allocator) !void {
        try self.line.encode(allocator, line);
        try self.code.append(allocator, @intFromEnum(OpCode.OP_CONSTANT_LONG));
        try self.constants.values.append(allocator, constant);
        const index: u24 = @intCast(self.constants.values.items.len - 1);
        try self.code.appendSlice(allocator, std.mem.toBytes(index)[0..3]);
    }

    pub fn deinit(self: *Chunk, allocator: std.mem.Allocator) void {
        self.constants.deinit(allocator);
        self.code.deinit(allocator);
    }

    pub fn getLine() void {
        // TODO: implement
    }

    pub fn disassemble(self: *const Chunk, name: []const u8) !void {
        print("== {s} ==\n", .{ name });
        var offset: usize = 0;
        var rleIdx: usize = 0;
        while (offset < self.code.items.len) {
            const opCode: OpCode = @enumFromInt(self.code.items[offset]);
            offset = try opCode.disassemble(self, offset, rleIdx);
            rleIdx += 1;
        }
    }
};


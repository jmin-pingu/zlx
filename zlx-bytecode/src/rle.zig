const std = @import("std");
const ArrayList = std.ArrayList;
const EncodingError = @import("error.zig").EncodingError;
const assert = std.debug.assert;


pub const RLE = struct {
    line: ArrayList(usize), 
    num: ArrayList(usize), 
    offset: usize,

    const Self = @This();

    pub fn init() RLE {
        return .{
            .line = .empty,
            .num = .empty,
            .offset = 0
        };
    }

    pub fn print(self: *Self) void {
        for (self.line.items, self.num.items) |i, n| {
            std.debug.print("value: {}, num: {}\n", .{i, n});
        }
    }

    pub fn encode(self: *Self, allocator: std.mem.Allocator, line: usize) !void {
        if (self.line.items.len != self.num.items.len) return EncodingError.InvalidState;

        if (self.line.items.len == 0) {
            try self.line.append(allocator, line);
            try self.num.append(allocator, 1);
            return;
        }
        
        if (self.line.getLast() == line) {
            self.num.items[self.num.items.len-1] += 1;
        } else {
            try self.line.append(allocator, line);
            try self.num.append(allocator, 1);
        }
    }

    pub fn decode(self: Self, index: usize) !usize {
        var curr: usize = 0;
        for (self.num.items, 0..) |count, idx| {
            if (curr <= index and index < curr + count) {
                return self.line.items[idx];
            }
            curr += count;
        }     
        return EncodingError.OutOfIndex;
    }

    pub fn decodeFirst(self: Self, index: usize) !?usize {
        var curr: usize = 0;
        for (self.num.items, 0..) |count, idx| {
            if (curr <= index and index < curr + count) {
                if (index == curr) return self.line.items[idx];
                return null;
            }
            curr += count;
        }     
        return EncodingError.OutOfIndex;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.line.deinit(allocator);
        self.num.deinit(allocator);
    }
};


test "basic functionality" {
    const allocator = std.testing.allocator;
    var rle = RLE.init();
    defer rle.deinit(allocator);
    try rle.encode(allocator, 0);
    try rle.encode(allocator, 1);
    try rle.encode(allocator, 2);
    try rle.encode(allocator, 3);
    rle.print();
}

test "error cases" {

}

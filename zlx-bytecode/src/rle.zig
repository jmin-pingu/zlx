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

    pub fn encode(self: *Self, allocator: std.mem.Allocator, line: usize) !void {
        if (self.line.items.len != self.num.items.len) {
            return EncodingError.InvalidState;
        }

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

    pub fn increment(self: *Self) bool {
        self.num += 1;
    }

    pub fn decode(self: Self, index: usize) !usize {
        var curr: usize = 0;
        for (self.num.items, 0..) |count, idx| {
            if (index < curr + count and index >= curr) {
                return self.line.items[idx];
            }
            curr += count;
        }     

        return EncodingError.OutOfIndex;
    }

    pub fn decodeFirst(self: Self, index: usize) !?usize {
        var curr: usize = 0;
        for (self.num.items, 0..) |count, idx| {
            if (index < curr + count and index >= curr) {
                if (index == curr) return self.line.items[idx];
                return null;
            }
            curr += count;
        }     
        return EncodingError.OutOfIndex;
    }
};


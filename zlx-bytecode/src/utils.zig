pub const Decrementer = struct {
    start: i64, 
    end: i64, 
    step: i64,
    const Self = @This();

    pub fn init(start: i64, end: i64, step: i64) Self {
        return .{ .start = start, .end = end, .step = step };
    }

    pub fn next(self: *Self) ?i64 {
        self.start += self.step; 
        if (self.start >= self.end) {
            return self.start; 
        } else {
            return null;
        }
    }
};

const std = @import("std");
const testing = std.testing;

test "decrementer steps once before yielding and stops below end" {
    var d = Decrementer.init(3, 0, -1);
    try testing.expectEqual(@as(?i64, 2), d.next());
    try testing.expectEqual(@as(?i64, 1), d.next());
    try testing.expectEqual(@as(?i64, 0), d.next());
    try testing.expectEqual(@as(?i64, null), d.next());
}

test "decrementer yields nothing when start equals end" {
    var d = Decrementer.init(0, 0, -1);
    try testing.expectEqual(@as(?i64, null), d.next());
}

test "decrementer honours arbitrary negative steps" {
    var d = Decrementer.init(10, 4, -3);
    try testing.expectEqual(@as(?i64, 7), d.next());
    try testing.expectEqual(@as(?i64, 4), d.next());
    try testing.expectEqual(@as(?i64, null), d.next());
}

test "decrementer walks local slots from top to bottom" {
    var seen: [4]i64 = undefined;
    var n: usize = 0;
    var d = Decrementer.init(4, 0, -1);
    while (d.next()) |idx| : (n += 1) seen[n] = idx;
    try testing.expectEqual(@as(usize, 4), n);
    try testing.expectEqualSlices(i64, &[_]i64{ 3, 2, 1, 0 }, &seen);
}

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

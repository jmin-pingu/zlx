const std = @import("std");
const Literal = @import("literal.zig").Literal;
const Error = @import("error.zig").Error;
const RuntimeError = @import("error.zig").RuntimeError;
const AutoHashMap = std.AutoHashMap;
const StringHashMap = std.StringHashMap;

pub const Environment = struct {
    values: StringHashMap(?Literal),

    pub fn init(allocator: std.mem.Allocator) Error!Environment {
        return Environment {
            .values = StringHashMap(?Literal).init(allocator)
        };
    }

    pub fn define(self: *Environment, name: []const u8, value: ?Literal, allocator: std.mem.Allocator) Error!void {
        const copied_name = try allocator.alloc(u8, name.len);
        @memcpy(copied_name, name);

        try self.values.put(copied_name, value);
    }

    pub fn get(self: Environment, name: []const u8) RuntimeError!Literal {
        if (self.values.get(name)) |value| {
            return value.?;
        } else {
            return RuntimeError.UndefinedVariable;
        }
    }
    
    pub fn print(self: Environment) void {
        var iterator = self.values.iterator();

        std.debug.print("printing environment: \n", .{});
        while (iterator.next()) |kv| {
            std.debug.print("\t{s}:{any}\n", .{kv.key_ptr.*, kv.value_ptr.*.?});
        }
    }
};


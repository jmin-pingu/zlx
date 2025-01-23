const std = @import("std");
const Literal = @import("literal.zig").Literal;
const Error = @import("error.zig").Error;
const err = @import("error.zig");
const Token = @import("token.zig").Token;
const RuntimeError = @import("error.zig").RuntimeError;
const AutoHashMap = std.AutoHashMap;
const StringHashMap = std.StringHashMap;

pub const Environment = struct {
    values: StringHashMap(?Literal),
    enclosing: ?*Environment = null,
 
    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Error!Environment {
        return Environment {
            .values = StringHashMap(?Literal).init(allocator),
            .enclosing = enclosing, 
        };
    }

    pub fn define(self: *Environment, name: Token, value: Literal, allocator: std.mem.Allocator) Error!void {
        // Issue: need to copy the data inside both value and name inside the hashmap.
        const copied_name = try allocator.alloc(u8, name.lexeme.len);
        @memcpy(copied_name, name.lexeme);
        try self.values.put(copied_name, value);
    }

    pub fn get(self: Environment, name: Token, allocator: std.mem.Allocator) RuntimeError!Literal {
        if (self.values.get(name.lexeme)) |value| {
            return value.?;
        } else if (self.enclosing) |parent_environment| {
            return parent_environment.get(name, allocator);
        } else {
            const error_msg = std.fmt.allocPrint(
                allocator, 
                "cannot access undefined variable '{s}' \nNOTE: Declare or initializer '{s}' with `var`", 
                .{name.lexeme, name.lexeme}
            ) catch return RuntimeError.AllocError;
            return err.runtime_error_msg(name.line, error_msg, RuntimeError.UndefinedVariable, allocator);
        }
    }
    
    pub fn assign(self: *Environment, name: Token, value: ?Literal, allocator: std.mem.Allocator) RuntimeError!void {
        // const copied_name = try allocator.alloc(u8, name.len);
        // @memcpy(copied_name, name);
        if (self.values.get(name.lexeme)) |_| {
            self.values.put(name.lexeme, value) catch return RuntimeError.AllocError;
        } else {
            const error_msg = std.fmt.allocPrint(
                allocator, 
                "cannot assign undefined variable '{s}'. \nNOTE: Declare or initializer '{s}' with `var`", 
                .{name.lexeme, name.lexeme}
            ) catch return RuntimeError.AllocError;
            return err.runtime_error_msg(name.line, error_msg, RuntimeError.UndefinedVariable, allocator);
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


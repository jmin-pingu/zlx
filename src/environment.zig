const std = @import("std");

const AutoHashMap = std.AutoHashMap;
const StringHashMap = std.StringHashMap;

const Object = @import("token/object.zig").Object;
const Token = @import("token/token.zig").Token;
const Function = @import("function.zig").Function;
const Callable = @import("callable.zig").Callable;

const err = @import("error.zig");
const Error = @import("error.zig").Error;
const RuntimeError = @import("error.zig").RuntimeError;

pub const Environment = struct {
    values: StringHashMap(?Object),
    enclosing: ?*Environment = null,
 
    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Error!Environment {
        return Environment {
            .values = StringHashMap(?Object).init(allocator),
            .enclosing = enclosing, 
        };
    }

    // TODO: reason about whether it makes sense to deinit the 
    pub fn deinit(self: *Environment) void {
        self.values.deinit(); 
        // if (self.enclosing) |enclosing| {
        //     enclosing.deinit(); 
        // }
    }

    pub fn define(self: *Environment, name: []const u8, value: ?Object, allocator: std.mem.Allocator) Error!void {
        const copied_name = try allocator.alloc(u8, name.len);
        @memcpy(copied_name, name);
        try self.values.put(copied_name, value);
    }

    pub fn get(self: Environment, name: []const u8, allocator: std.mem.Allocator) RuntimeError!Object {
        if (self.values.get(name)) |optional| {
            if (optional) |value| {
                return value;
            } else {
                const error_msg = std.fmt.allocPrint(
                    allocator, 
                    "the variable '{s}' is uninitialized", 
                    .{name}
                ) catch return RuntimeError.AllocError;
                return err.runtime_error_msg(null, error_msg, RuntimeError.UninitializedVariable, allocator);
            }
        } else if (self.enclosing) |parent_environment| {
            return parent_environment.get(name, allocator);
        } else {
            const error_msg = std.fmt.allocPrint(
                allocator, 
                "cannot access undeclared variable '{s}' \nNOTE: Declare or initialize '{s}' with `var`", 
                .{name, name}
            ) catch return RuntimeError.AllocError;
            return err.runtime_error_msg(null, error_msg, RuntimeError.UndeclaredVariable, allocator);
        }
    }
    
    pub fn assign(self: *Environment, name: Token, value: Object, allocator: std.mem.Allocator) RuntimeError!void {
        // const copied_name = try allocator.alloc(u8, name.len);
        // @memcpy(copied_name, name);
        if (self.values.get(name.lexeme)) |_| {
            self.values.put(name.lexeme, value) catch return RuntimeError.AllocError;
        } else if (self.enclosing) |parent_environment| {
            try parent_environment.assign(name, value, allocator);
        } else {
            const error_msg = std.fmt.allocPrint(
                allocator, 
                "cannot assign undeclared variable '{s}'. \nNOTE: Declare or initialize '{s}' with `var`", 
                .{name.lexeme, name.lexeme}
            ) catch return RuntimeError.AllocError;
            return err.runtime_error_msg(name.line, error_msg, RuntimeError.UndeclaredVariable, allocator);
        }
    }

    pub fn print(self: Environment, allocator: std.mem.Allocator) RuntimeError!void {
        std.debug.print("-----\nenvs:\n", .{});

        try self.blockPrint(0, allocator);
    }

    // TODO: figure out backend of blockPrint
    fn blockPrint(self: Environment, tabs: u8, allocator: std.mem.Allocator) RuntimeError!void {
        var spacing: []const u8 = "  "; 
        var i : u8 = 0;

        if (tabs == 0) {
            std.debug.print("{c}\n", .{'{'});
        }

        while (i < tabs) {
            spacing = std.fmt.allocPrint(
                allocator, 
                "{s}{s}", 
                .{spacing, "  "}
            ) catch return RuntimeError.AllocError;
            i += 1;
        }

        var iterator = self.values.iterator();
        while (iterator.next()) |kv| {
            if (kv.value_ptr.*) |value| {
                std.debug.print("{s}{s}:{}\n", .{spacing, kv.key_ptr.*, value});

            } else {
                std.debug.print("{s}{s}:{s}\n", .{spacing, kv.key_ptr.*, "uninit"});
            }
        } 

        if (self.enclosing) |parent_environment| {
            try parent_environment.blockPrint(tabs + 1, allocator);
        } else {
            std.debug.print("{c}\n", .{'}'});
        }
    }
};


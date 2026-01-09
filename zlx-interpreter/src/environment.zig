const std = @import("std");

const AutoHashMap = std.AutoHashMap;
const StringHashMap = std.StringHashMap;

const Object = @import("primitives/object.zig").Object;
const Token = @import("primitives/token.zig").Token;
const Function = @import("primitives/callable/function.zig").Function;

const err = @import("error.zig");
const VariableError = @import("error.zig").VariableError;
const RuntimeError = @import("error.zig").RuntimeError;
const AllocationError = @import("error.zig").AllocationError;
const EnvironmentError = @import("error.zig").EnvironmentError;

pub const Environment = struct {
    values: StringHashMap(?Object),
    enclosing: ?*Environment = null,
 
    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Environment {
        return Environment {
            .values = StringHashMap(?Object).init(allocator),
            .enclosing = enclosing, 
        };
    }

    // TODO: implement deinit()
    pub fn deinit(self: *Environment) void {
        self.values.deinit(); 
    }

    pub fn getAt(self: *Environment, distance: usize, name: []const u8, allocator: std.mem.Allocator) EnvironmentError!Object{
        return try self.ancestor(distance).get(name, allocator);
    }

    fn ancestor(self: *Environment, distance: usize) *Environment{
        var ancestor_env = self;
        for (0..distance) |_| {
            if (ancestor_env.enclosing != null) {
                ancestor_env = ancestor_env.enclosing.?;
            } else {
                unreachable;
            }
        }
        return ancestor_env;
    }
    
    pub fn define(
        self: *Environment, 
        name: []const u8, 
        value: ?Object, 
        allocator: std.mem.Allocator
    ) AllocationError!void {
        const copied_name = allocator.alloc(u8, name.len) catch return err.outOfMemory();
        @memcpy(copied_name, name);
        self.values.put(copied_name, value) catch return err.outOfMemory();
    }

    pub fn get(self: Environment, name: []const u8, allocator: std.mem.Allocator) EnvironmentError!Object {
        if (self.values.get(name)) |optional| {
            if (optional) |value| {
                return value;
            } else {
                const error_msg = std.fmt.allocPrint(
                    allocator, 
                    "the variable '{s}' is uninitialized", 
                    .{name}
                ) catch return err.outOfMemory();
                return err.errorMessage(EnvironmentError, null, error_msg, EnvironmentError.UninitalizedObject, allocator);
            }
        } else if (self.enclosing) |parent_environment| {
            return parent_environment.get(name, allocator);
        } else {
            const error_msg = std.fmt.allocPrint(
                allocator, 
                "cannot access undeclared object '{s}' \nNOTE: Declare or initialize '{s}' with `var` or `fun`", 
                .{name, name}
            ) catch return err.outOfMemory();
            return err.errorMessage(EnvironmentError, null, error_msg, EnvironmentError.UndeclaredObject, allocator);
        }
    }

    pub fn assignAt(self: *Environment, distance: usize, name: Token, value: Object, allocator: std.mem.Allocator) VariableError!void {
        if (self.ancestor(distance).values.get(name.lexeme)) |_| {
            self.ancestor(distance).values.put(name.lexeme, value) catch return err.outOfMemory();
        } else {
            const error_msg = std.fmt.allocPrint(
                allocator, 
                "cannot assign undeclared variable '{s}'. \nNOTE: Declare or initialize '{s}' with `var`", 
                .{name.lexeme, name.lexeme}
            ) catch return err.outOfMemory();
            return err.errorMessage(VariableError, name.line, error_msg, VariableError.UndeclaredVariable, allocator);
        }
    }

    
    pub fn assign(
        self: *Environment, 
        name: Token, 
        value: Object, 
        allocator: std.mem.Allocator
    ) VariableError!void {
        if (self.values.get(name.lexeme)) |_| {
            self.values.put(name.lexeme, value) catch return err.outOfMemory();
        } else if (self.enclosing) |parent_environment| {
            try parent_environment.assign(name, value, allocator);
        } else {
            const error_msg = std.fmt.allocPrint(
                allocator, 
                "cannot assign undeclared variable '{s}'. \nNOTE: Declare or initialize '{s}' with `var`", 
                .{name.lexeme, name.lexeme}
            ) catch return err.outOfMemory();
            return err.errorMessage(VariableError, name.line, error_msg, VariableError.UndeclaredVariable, allocator);
        }
    }

    // TODO: reimplement print(), current issue with infinite recursion
    pub fn print(self: Environment, allocator: std.mem.Allocator) AllocationError!void {
        std.debug.print("-----\nenvs:\n", .{});

        try self.blockPrint(0, allocator);
    }

    fn blockPrint(self: Environment, tabs: u8, allocator: std.mem.Allocator) AllocationError!void {
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
            ) catch return err.outOfMemory();
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


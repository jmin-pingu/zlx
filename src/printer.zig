const std = @import("std");
const Token = @import("token.zig").Token;
const LiteralValue = @import("literal.zig").Literal;
const e= @import("expr.zig");
const Expr= @import("expr.zig").Expr;
const Error= @import("error.zig").Error;
const Visitor= @import("expr.zig").Visitor;

// Helpful Articles
// - https://www.nmichaels.org/zig/interfaces.html 
// - https://zig.news/kristoff/easy-interfaces-with-zig-0100-2hc5
// - https://www.ryanliptak.com/blog/zig-fieldparentptr-for-dumbos/
pub const AstPrinter = struct {
    const Self = @This();
    const T: type = []const u8;
    allocator: std.mem.Allocator,

    pub fn print(self: *Self, expr: *const Expr) Error!T {
        const visitor = self.init_visitor();
        return expr.accept(T, visitor);
    }

    /// 
    pub fn visitBinaryExpr(self: *Self, expr: *const e.Binary) Error!T {
        var parsed_expr = self.allocator.alloc(*const Expr, 2) catch return Error.AllocError;
        parsed_expr[0] = expr.left;
        parsed_expr[1] = expr.right;
        return try self.parenthesize(expr.operator.lexeme, parsed_expr);
    }

    /// 
    pub fn visitGroupingExpr(self: *Self, expr: *const e.Grouping) Error!T {
        var parsed_expr = self.allocator.alloc(*const Expr, 1)  catch return Error.AllocError;
        parsed_expr[0] = expr.expression;
        return try self.parenthesize("group", parsed_expr);
    }

    /// 
    pub fn visitLiteralExpr(self: *Self, expr: *const e.Literal) Error!T {
        switch (expr.value) {
            .Number => |value| {
                return std.fmt.allocPrint(
                    self.allocator, 
                    "{d}", 
                    .{value}
                ) catch return Error.AllocError;
            },
            .String, .Identifier => |value| {
                return std.fmt.allocPrint(
                    self.allocator, 
                    "{s}", 
                    .{value}
                ) catch return Error.AllocError;
            },
            .Nil => {
                return "null";
            },
            .Bool => |value| {
                return std.fmt.allocPrint(
                    self.allocator, 
                    "{any}", 
                    .{value}
                ) catch return Error.AllocError;
            },
        }
    }

    /// 
    pub fn visitUnaryExpr(self: *Self, expr: *const e.Unary) Error!T {
        var parsed_expr = self.allocator.alloc(*const Expr, 1) catch return Error.AllocError;
        parsed_expr[0] = expr.right;
        return try self.parenthesize(expr.operator.lexeme, parsed_expr);
    }

    pub fn init_visitor(self: *Self) Visitor(T) {
        return Visitor(T).init(self);
    }

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator=allocator};
    }

    // TODO: need to double check parenthesize definition
    fn parenthesize(self: *Self, name: []const u8, exprs: []*const Expr) Error!T {
        var interior: T = "";
        const visitor = self.init_visitor();

        for (exprs) |expr| {
            const parenthesized = expr.accept(T, visitor) catch |err| {return err;};
            interior = std.fmt.allocPrint(
                self.allocator, 
                "{s} {s}", 
                .{interior, parenthesized}
            ) catch return Error.AllocError;
        }
        return std.fmt.allocPrint(self.allocator, "({s}{s} )", .{name, interior}) catch return Error.AllocError;
    }
};


// TODO: redefine test suite
test "test" {

}

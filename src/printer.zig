const std = @import("std");

const e= @import("primitives/expr.zig");
const Expr= e.Expr;
const ExprVisitor= e.Visitor;
 
const Error= @import("error.zig").Error;

// Helpful Articles
// - https://www.nmichaels.org/zig/interfaces.html 
// - https://zig.news/kristoff/easy-interfaces-with-zig-0100-2hc5
// - https://www.ryanliptak.com/blog/zig-fieldparentptr-for-dumbos/
pub const AstPrinter = struct {
    const Self = @This();
    const T: type = Error![]const u8;
    allocator: std.mem.Allocator,

    pub fn print(self: *Self, expr: *const Expr) T {
        const visitor = self.init_visitor();
        return expr.accept(T, visitor);
    }

    /// 
    pub fn visitBinaryExpr(self: *Self, expr: e.Binary) T {
        var parsed_expr = self.allocator.alloc(*const Expr, 2) catch return Error.AllocError;
        parsed_expr[0] = expr.left;
        parsed_expr[1] = expr.right;
        return try self.parenthesize(expr.operator.lexeme, parsed_expr);
    }

    /// 
    pub fn visitGroupingExpr(self: *Self, expr: e.Grouping) T {
        var parsed_expr = self.allocator.alloc(*const Expr, 1)  catch return Error.AllocError;
        parsed_expr[0] = expr.expression;
        return try self.parenthesize("group", parsed_expr);
    }

    /// 
    pub fn visitLiteralExpr(self: *Self, expr: e.Literal) T {
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
    pub fn visitUnaryExpr(self: *Self, expr: e.Unary) T {
        var parsed_expr = self.allocator.alloc(*const Expr, 1) catch return Error.AllocError;
        parsed_expr[0] = expr.right;
        return try self.parenthesize(expr.operator.lexeme, parsed_expr);
    }

    pub fn init_visitor(self: *Self) ExprVisitor(T) {
        return ExprVisitor(T).init(self);
    }

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator=allocator};
    }

    // 
    fn parenthesize(self: *Self, name: []const u8, exprs: []*const Expr) T {
        var interior: []const u8 = "";
        // NOTE: is this unnecessary? Is there a way to directly call the visitor methods
        const visitor = self.init_visitor();

        for (exprs) |expr| {
            const parenthesized: []const u8 = try expr.accept(T, visitor);
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

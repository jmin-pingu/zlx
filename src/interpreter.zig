const std = @import("std");
const e= @import("expr.zig");
const s = @import("stmt.zig");
const Token = @import("token.zig").Token;
const TokenType = @import("token_type.zig").TokenType;
const ArrayList = std.ArrayList;

const ExprVisitor = @import("visitor.zig").ExprVisitor;
const StmtVisitor = @import("visitor.zig").StmtVisitor;
const Literal= @import("literal.zig").Literal;
const Tag = @import("literal.zig").Tag;

const RuntimeError = @import("error.zig").RuntimeError;
const Error = @import("error.zig").Error;

// TODO: rethink this struct and whether I want the `Visitor`s as fields
pub const Interpreter = struct {
    const Self = @This();
    const T: type = RuntimeError!Literal;
    const stmt_T: type = RuntimeError!void;
    allocator: std.mem.Allocator,

    // public facing methods 
    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator=allocator};
    }

    // pub fn interpret(self: *Self, expr: *const e.Expr) Error!void {
    //     const value = self.evaluate(expr) catch return Error.RuntimeError;
    //     std.debug.print("{s}\n", .{value.to_string(self.allocator) catch return RuntimeError.AllocError});
    // }
    
    pub fn interpret(self: *Self, statements: ArrayList(s.Stmt)) Error!void {
        for (statements.items) |statement| {
            self.execute(statement) catch return Error.RuntimeError;
        }     
    }

    // private methods
    fn evaluate(self: *Self, expr: *const e.Expr) T {
        const visitor = self.initExprVisitor();
        return expr.accept(T, visitor);
    }

    fn execute(self: *Self, stmt: s.Stmt) stmt_T {
        const visitor = self.initStmtVisitor();
        return stmt.accept(stmt_T, visitor);
    }

    // interface initialization methods
    fn initExprVisitor(self: *Self) ExprVisitor(T) {
        return ExprVisitor(T).init(self);
    }

    fn initStmtVisitor(self: *Self) StmtVisitor(stmt_T) {
        return StmtVisitor(stmt_T).init(self);
    }

    // visitorStmt logic
    pub fn visitExpressionStmt(self: *Self, stmt: s.Expression) stmt_T {
        _ = self.evaluate(stmt.expression) catch |err| return err;
    }

    pub fn visitPrintStmt(self: *Self, stmt: s.Print) stmt_T {
        const value = try self.evaluate(stmt.expression);
        std.debug.print("{s}\n", .{try value.to_string(self.allocator)});
    }

    // visitorExpr logic
    pub fn visitBinaryExpr(self: *Self, expr: *const e.Binary) T {
        // Now evaluate should return a RuntimeError
        const left = try self.evaluate(expr.left);
        // error_msg(line_number: usize, message: []const u8, err: Error, allocator: std.mem.Allocator) Error {
        const right = try self.evaluate(expr.right);

        return switch (expr.operator.ttype) {
            TokenType.MINUS => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = left.Number - right.Number};
            },
            TokenType.SLASH => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = left.Number / right.Number};

            },
            TokenType.STAR => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = left.Number * right.Number};
            },
            TokenType.PLUS => {
                // Check if both string OR both numeric
                if (!left.same_tags(right, Tag.String) and !left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                if (left.check_tag(Tag.String)) {
                    const new_string = std.fmt.allocPrint(
                        self.allocator, 
                        "{s}{s}", 
                        .{left.String, right.String}
                    ) catch return RuntimeError.AllocError;
                    return Literal{ .String = new_string};

                } else {
                    return Literal{ .Number = left.Number + right.Number};
                }
            },
            TokenType.GREATER => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Bool = left.Number > right.Number};
            },
            TokenType.GREATER_EQUAL => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Bool = left.Number >= right.Number};
            },
            TokenType.LESS => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Bool = left.Number < right.Number};
            },
            TokenType.LESS_EQUAL => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Bool = left.Number <= right.Number};
            },
            TokenType.BANG_EQUAL => {
                return Literal{ .Bool = !left.equals(right)};
            },
            TokenType.EQUAL_EQUAL => {
                return Literal{ .Bool = left.equals(right)};
            },
            else => {
                return RuntimeError.OperatorError;
            }
        } catch |runtime_err| {
                return runtime_error_msg(runtime_err, e.operator , self.allocator);
        };
    }

    pub fn visitGroupingExpr(self: *Self, expr: *const e.Grouping) T {
        return self.evaluate(expr.expression);
    }

    pub fn visitLiteralExpr(self: *Self, expr: *const e.Literal) T {
        _ = self;
        return expr.value;
    }

    pub fn visitUnaryExpr(self: *Self, expr: *const e.Unary) T {
        const right = try self.evaluate(expr.right);
        return switch (expr.operator.ttype) {
            TokenType.MINUS => {
                // TODO: check numeric
                if (!right.check_tag(Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = -right.Number};
            },
            TokenType.BANG => {
                return Literal{ .Bool = !right.is_truthy()};
            },
            else => {
                return RuntimeError.OperatorError;
            }
        } catch |runtime_err| { 
            return runtime_error_msg(runtime_err, expr.operator);
        };
    }


    fn runtime_error_msg(self: *Self, runtime_err: RuntimeError, token: Token) RuntimeError {
        const message = "";
        std.debug.print("{s}", .{
            std.fmt.allocPrint(
                self.allocator, 
                "[line: {d}] RuntimeError: {!} {s} {s}\n", 
                .{token.line, runtime_err, message, token.lexeme}
            ) catch return Error.AllocError}
        );
        return Error.RuntimeError; 
    }
};


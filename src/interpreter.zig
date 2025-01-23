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
const Environment = @import("environment.zig").Environment;

const RuntimeError = @import("error.zig").RuntimeError;
const Error = @import("error.zig").Error;
const err = @import("error.zig");

// TODO: rethink this struct and whether I want the `Visitor`s as fields
pub const Interpreter = struct {
    const Self = @This();
    const T: type = RuntimeError!Literal;
    const stmt_T: type = RuntimeError!void;
    allocator: std.mem.Allocator,
    environment: Environment,

    // public facing methods 
    pub fn init(allocator: std.mem.Allocator) Error!Self {
        // when initializing the interpreter, the environment is the root environment
        return .{ 
            .environment=try Environment.init(allocator, null),
            .allocator=allocator
        };
    }

    // pub fn interpret(self: *Self, expr: *const e.Expr) Error!void {
    //     const value = self.evaluate(expr) catch return Error.RuntimeError;
    //     std.debug.print("{s}\n", .{value.to_string(self.allocator) catch return RuntimeError.AllocError});
    // }
    
    pub fn interpret(self: *Self, statements: *ArrayList(s.Stmt)) Error!void {
        while (statements.items.len > 0) {
            const statement = statements.orderedRemove(0);
            self.execute(statement) catch |runtime_error| {
                if (runtime_error == RuntimeError.AllocError) {
                    // Do nothing upon allocation error
                    err.runtime_error_msg(null, "allocation error at runtime", runtime_error, self.allocator) catch {};
                }
                return Error.RuntimeError;
            };
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
    pub fn visitBlockStmt(self: *Self, stmt: s.Block) stmt_T {
        _ = self.executeBlock(
            stmt.statements, 
            Environment.init(self.allocator, &self.environment) catch return RuntimeError.AllocError
        ) catch |runtime_err| return runtime_err;
    }

    fn executeBlock(self: *Self, statements: ArrayList(s.Stmt), environment: Environment) RuntimeError!void {
        const prev = self.environment;
        // Restore environment upon executing block
        defer self.environment = prev;
        errdefer self.environment = prev;

        self.environment = environment;
        for (statements.items) |stmt| {
            try self.execute(stmt);
        }
    }

    pub fn visitExpressionStmt(self: *Self, stmt: s.Expression) stmt_T {
        _ = self.evaluate(stmt.expression) catch |runtime_err| return runtime_err;
    }

    pub fn visitPrintStmt(self: *Self, stmt: s.Print) stmt_T {
        const value = try self.evaluate(stmt.expression);
        std.debug.print("{s}\n", .{try value.to_string(self.allocator)});
    }

    pub fn visitVarStmt(self: *Self, stmt: s.Var) stmt_T {
        const value = self.allocator.create(Literal) catch return RuntimeError.AllocError;
        if (stmt.initializer) |checked_initializer| {
            value.* = try self.evaluate(checked_initializer);
        }
        self.environment.define(stmt.name, value.*, self.allocator) catch return RuntimeError.AllocError;
    }

    // visitorExpr logic
    pub fn visitAssignExpr(self: *Self, expr: *const e.Assign) T {
        const value = try self.evaluate(expr.value);
        try self.environment.assign(expr.name, value, self.allocator);
        return value;
    }

    pub fn visitVarExpr(self: *Self, expr: *const e.Var) T {
        return try self.environment.get(expr.name, self.allocator);
    }

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
        } catch |runtime_err| switch (runtime_err) {
            .OperatorError => {
                std.debug.print("reached\n", .{});
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "operator {s} is not a binary operator", 
                    .{expr.operator.lexeme}
                ) catch return RuntimeError.AllocError;
                std.debug.print("reached\n", .{});
                return err.runtime_error_msg(expr.operator.line, err_msg, runtime_err, self.allocator);
            },
            .OperandError => {
                std.debug.print("reached\n", .{});
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "operands {any} and {any} are not compatible", 
                    .{left, right}
                ) catch return RuntimeError.AllocError;
                std.debug.print("reached\n", .{});
                return err.runtime_error_msg(expr.operator.line, err_msg, runtime_err, self.allocator);
            }, 
            else => return runtime_err
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
        } catch |runtime_err| switch (runtime_err) {
            .OperatorError => {
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "operator {s} is not a unary operator", 
                    .{expr.operator.lexeme}
                ) catch return RuntimeError.AllocError;
                return err.runtime_error_msg(expr.operator.line, err_msg, runtime_err, self.allocator);
            },
            .OperandError => {
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "operand {any} is not compatible with operator {s}", 
                    .{right, expr.operator.lexeme}
                ) catch return RuntimeError.AllocError;
                return err.runtime_error_msg(expr.operator.line, err_msg, runtime_err, self.allocator);
            }, 
            else => return runtime_err
        };
    }
};


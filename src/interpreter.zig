const std = @import("std");

const ArrayList = std.ArrayList;

const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const Literal= @import("token/literal.zig").Literal;
const Tag = @import("token/literal.zig").Tag;

const e= @import("expr.zig");
const ExprVisitor = @import("expr.zig").Visitor;

const s = @import("stmt.zig");
const StmtVisitor = @import("stmt.zig").Visitor;

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

    // TODO: note the interpreter owns all subsequent types and thus the responsibility of deallocating heap memory should be with respect to the interpreter
    // public facing methods 
    pub fn init(allocator: std.mem.Allocator) Error!Self {
        // when initializing the interpreter, the environment is the root environment
        return .{ 
            .environment= try Environment.init(allocator, null),
            .allocator=allocator
        };
    }

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
            // Uncomment for debugging environments
            // self.environment.print(self.allocator) catch return Error.AllocError;
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

    fn initExprVisitor(self: *Self) ExprVisitor(T) {
        return ExprVisitor(T).init(self);
    }

    fn initStmtVisitor(self: *Self) StmtVisitor(stmt_T) {
        return StmtVisitor(stmt_T).init(self);
    }

    // visitorStmt logic
    pub fn visitIfStmt(self: *Self, stmt: s.If) stmt_T {
        if ((try self.evaluate(stmt.condition)).isTruthy()) {
            try self.execute(stmt.then_branch.*);
        } else if (stmt.else_branch) |else_branch| {
            try self.execute(else_branch.*);
        }     
    }

    pub fn visitBlockStmt(self: *Self, stmt: s.Block) stmt_T {
        const enclosed_environment = self.allocator.create(Environment) catch return RuntimeError.AllocError;
        enclosed_environment.* = self.environment;

        const block_environment = Environment.init(
            self.allocator, 
           enclosed_environment 
        ) catch return RuntimeError.AllocError;
        
        _ = self.executeBlock(
            stmt.statements, 
            block_environment
        ) catch |runtime_err| return runtime_err;
    }

    fn executeBlock(self: *Self, statements: ArrayList(s.Stmt), environment: Environment) RuntimeError!void { 
        const parent_environment = self.environment; // move
        // Restore environment upon executing block
        defer self.environment = parent_environment; 
        errdefer self.environment = parent_environment;

        self.environment = environment;
        for (statements.items) |stmt| {
            try self.execute(stmt);
        }
    }

    pub fn visitExpressionStmt(self: *Self, stmt: s.Expression) stmt_T {
        const value = self.evaluate(stmt.expression) catch |runtime_err| return runtime_err;
        std.debug.print("{s}\n", .{try value.to_string(self.allocator)});
    }

    pub fn visitPrintStmt(self: *Self, stmt: s.Print) stmt_T {
        const value = try self.evaluate(stmt.expression);
        std.debug.print("{s}\n", .{try value.to_string(self.allocator)});
    }

    pub fn visitVarStmt(self: *Self, stmt: s.Var) stmt_T {
        const value = self.allocator.create(Literal) catch return RuntimeError.AllocError;
        if (stmt.initializer) |checked_initializer| {
            // Instantiation
            value.* = try self.evaluate(checked_initializer);
            self.environment.define(stmt.name, value.*, self.allocator) catch return RuntimeError.AllocError;
        } else {
            // Declaration
            self.environment.define(stmt.name, null, self.allocator) catch return RuntimeError.AllocError;
        }
    }

    // visitorExpr logic
    pub fn visitAssignExpr(self: *Self, expr: e.Assign) T {
        const value = try self.evaluate(expr.value);
        try self.environment.assign(expr.name, value, self.allocator);
        return value;
    }

    pub fn visitVarExpr(self: *Self, expr: e.Var) T {
        const optional = try self.environment.get(expr.name, self.allocator);
        if (optional) |value| {
            return value; 
        } else {
            return RuntimeError.UndeclaredVariable; 
        }
    }

    pub fn visitLogicalExpr(self: *Self, expr: e.Logical) T {
        const left = try self.evaluate(expr.left);
        switch (expr.operator.ttype) {
            TokenType.OR => if (left.isTruthy()) return left,
            TokenType.AND => if (!left.isTruthy()) return left, 
            else => unreachable
        }
        return self.evaluate(expr.right);
    }

    pub fn visitBinaryExpr(self: *Self, expr: e.Binary) T {
        const left = try self.evaluate(expr.left);
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

    pub fn visitGroupingExpr(self: *Self, expr: e.Grouping) T {
        return self.evaluate(expr.expression);
    }

    pub fn visitLiteralExpr(self: *Self, expr: e.Literal) T {
        _ = self;
        return expr.value;
    }

    pub fn visitUnaryExpr(self: *Self, expr: e.Unary) T {
        const right = try self.evaluate(expr.right);
        return switch (expr.operator.ttype) {
            TokenType.MINUS => {
                // TODO: check numeric
                if (!right.check_tag(Tag.Number)) return RuntimeError.OperandError;
                return Literal{ .Number = -right.Number};
            },
            TokenType.BANG => {
                return Literal{ .Bool = !right.isTruthy()};
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

test "fix" {

}

const std = @import("std");

const ArrayList = std.ArrayList;

const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const Object= @import("token/object.zig").Object;
const Tag = @import("token/object.zig").Tag;

const e= @import("expr.zig");
const ExprVisitor = @import("expr.zig").Visitor;

const s = @import("stmt.zig");
const StmtVisitor = @import("stmt.zig").Visitor;

const Function = @import("function.zig").Function;
const Callable = @import("callable.zig").Callable;

const Environment = @import("environment.zig").Environment;

const RuntimeError = @import("error.zig").RuntimeError;
const Error = @import("error.zig").Error;
const err = @import("error.zig");

// TODO: rethink this struct and whether I want the `Visitor`s as fields
pub const Interpreter = struct {
    const Self = @This();
    const T: type = RuntimeError!Object;
    const stmt_T: type = RuntimeError!void;
    allocator: std.mem.Allocator,
    environment: Environment,
    globals: *Environment,

    // TODO: note the interpreter owns all subsequent types and thus the responsibility of deallocating heap memory should be with respect to the interpreter
    pub fn init(allocator: std.mem.Allocator) Error!Self {
        // when initializing the interpreter, the environment is the root environment
        var env = try Environment.init(allocator, null);
        const temp = .{ 
            .environment= env,
            .globals = &env,
            .allocator=allocator
        };
        // TODO: stuff native functions into global scope
        // TODO: need to figure out how to incorporate native functions
        // const Clock = struct {
        //     const S = @This();

        //     pub fn arity(self: *S) usize {
        //         _ = self;
        //         return 0;
        //     }

        //     pub fn call(self: *S, interpreter: *Interpreter, arguments: ArrayList(Object)) Error!Object {
        //         _ = self;
        //         _ = interpreter;
        //         _ = arguments;
        //         std.debug.print("{d}", .{@divFloor(std.time.milliTimestamp(), 1000)});
        //         return Object{.Nil = null};
        //     }

        //     pub fn toString(self: *S) []const u8{
        //         _ = self;
        //         return "<native fn>";
        //     }


        //     pub fn initCallable(self: *S) Callable() {
        //         return Callable().init(self);
        //     }
        // };
        // var clock = Clock{};

        // var fn_ref = try allocator.create(Function);
        // fn_ref.* = Function.init(null, allocator) catch return Error.AllocError;
        // fn_ref.callable = clock.initCallable();
        // try temp.globals.define("clock", Object{.Function = fn_ref}, allocator);
        return temp;
    }

    pub fn interpret(self: *Self, statements: *ArrayList(s.Stmt)) Error!void {
        while (statements.items.len > 0) {
            const statement = statements.orderedRemove(0);
            // std.debug.print("interpreting {any}\n", .{statement});
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

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    // private methods
    fn evaluate(self: *Self, expr: *e.Expr) T {
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
    pub fn visitFunctionStmt(self: *Self, stmt: s.Function) stmt_T {
        const fn_ref = self.allocator.create(Function) catch return RuntimeError.AllocError;
        fn_ref.* = Function.init(stmt, self.allocator) catch return RuntimeError.AllocError;
        // const callable_ref = self.allocator.create(Callable()) catch return RuntimeError.AllocError;
       
        std.debug.print("declaring function {s}, {any}\n\n", .{fn_ref.initCallable().toString() catch return RuntimeError.AllocError, fn_ref.initCallable().arity()});
        self.environment.define(stmt.name.lexeme, Object{.Function=fn_ref}, self.allocator) catch return RuntimeError.AllocError;
    }

    pub fn visitBreakStmt(self: *Self, stmt: s.Break) stmt_T {
        stmt.associated_condition.* = e.Literal.new(Object{.Bool = false}, self.allocator).*;
        return;
    }

    pub fn visitWhileStmt(self: *Self, stmt: s.While) stmt_T {
        while ((try self.evaluate(stmt.condition)).isTruthy()) {
            try self.execute(stmt.body.*);
        }
    }

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

    pub fn executeBlock(self: *Self, statements: ArrayList(s.Stmt), environment: Environment) RuntimeError!void { 
        const parent_environment = self.environment; // move
        defer self.environment = parent_environment; 
        errdefer self.environment = parent_environment;

        self.environment = environment;
        for (statements.items) |stmt| {
            // TODO: issue, if we execute a break, how do we jump out?
            switch (stmt) {
                .@"break" => {
                    try self.execute(stmt);
                    break;
                },
                else => {
                    std.debug.print("{any}\n", .{stmt});
                    try self.execute(stmt);
                }
            }
        }
    }

    // TODO: what if we change stmt to return value but don't do anything with it for debugging?
    pub fn visitExpressionStmt(self: *Self, stmt: s.Expression) stmt_T {
        const value = self.evaluate(stmt.expression) catch |runtime_err| return runtime_err;
        switch (stmt.expression.*) {
            .assign => {},
            else => {
                const literal = try value.to_string(self.allocator);
                std.debug.print("{s}\n", .{literal});
            }
        }
    }

    pub fn visitPrintStmt(self: *Self, stmt: s.Print) stmt_T {
        const value = try self.evaluate(stmt.expression);
        std.debug.print("{s}\n", .{try value.to_string(self.allocator)});
    }

    pub fn visitVarStmt(self: *Self, stmt: s.Var) stmt_T {
        const value = self.allocator.create(Object) catch return RuntimeError.AllocError;
        if (stmt.initializer) |checked_initializer| {
            // Instantiation
            value.* = try self.evaluate(checked_initializer);
            self.environment.define(stmt.name.lexeme, value.*, self.allocator) catch return RuntimeError.AllocError;
        } else {
            // Declaration
            self.environment.define(stmt.name.lexeme, null, self.allocator) catch return RuntimeError.AllocError;
        }
    }

    // visitorExpr logic
    // TODO: double-check implementation here 
    pub fn visitCallExpr(self: *Self, expr: e.Call) T {
        const callee = try self.evaluate(expr.callee);
        var arguments = ArrayList(Object).init(self.allocator);
        for (expr.arguments.items) |arg| {
            arguments.append(try self.evaluate(arg)) catch return RuntimeError.AllocError;
        }

        const function = switch (callee) {
            .Function => |value| value.initCallable(),
            else => {
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "function not defined", 
                    .{}
                ) catch return RuntimeError.AllocError;
                return err.runtime_error_msg(expr.paren.line, err_msg, RuntimeError.TooManyArguments, self.allocator);
            },
        };

        std.debug.print("FUNC_CALL: {s}, arity: {d}, args: {any}\n", .{function.toString() catch return RuntimeError.AllocError, function.arity(), arguments}) ;
        if (arguments.items.len != function.arity()) {
            const err_msg = std.fmt.allocPrint(
                self.allocator, 
                "expected {d} arguments, got {d}", 
                .{arguments.items.len, function.arity()}
            ) catch return RuntimeError.AllocError;
            return err.runtime_error_msg(expr.paren.line, err_msg, RuntimeError.TooManyArguments, self.allocator);
             
        }
        return function.call(self, arguments) catch return RuntimeError.FunctionCallError;

    }

    pub fn visitAssignExpr(self: *Self, expr: e.Assign) T {
        const value = try self.evaluate(expr.value);
        try self.environment.assign(expr.name, value, self.allocator);
        return value;
    }

    pub fn visitVarExpr(self: *Self, expr: e.Var) T {
        std.debug.print("getting: {s}\n", .{expr.name.lexeme});
        const variable = try self.environment.get(expr.name.lexeme, self.allocator);
        return variable;
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
                return Object{ .Number = left.Number - right.Number};
            },
            TokenType.SLASH => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Number = left.Number / right.Number};

            },
            TokenType.STAR => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Number = left.Number * right.Number};
            },
            TokenType.PLUS => {
                if (!left.same_tags(right, Tag.String) and !left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                if (left.check_tag(Tag.String)) {
                    const new_string = std.fmt.allocPrint(
                        self.allocator, 
                        "{s}{s}", 
                        .{left.String, right.String}
                    ) catch return RuntimeError.AllocError;
                    return Object{ .String = new_string};

                } else {
                    return Object{ .Number = left.Number + right.Number};
                }
            },
            TokenType.GREATER => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Bool = left.Number > right.Number};
            },
            TokenType.GREATER_EQUAL => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Bool = left.Number >= right.Number};
            },
            TokenType.LESS => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Bool = left.Number < right.Number};
            },
            TokenType.LESS_EQUAL => {
                if (!left.same_tags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Bool = left.Number <= right.Number};
            },
            TokenType.BANG_EQUAL => {
                return Object{ .Bool = !left.equals(right)};
            },
            TokenType.EQUAL_EQUAL => {
                return Object{ .Bool = left.equals(right)};
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
                return Object{ .Number = -right.Number};
            },
            TokenType.BANG => {
                return Object{ .Bool = !right.isTruthy()};
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

test "scanner" {
    const Scanner = @import("scanner.zig").Scanner;
    const Parser = @import("parser.zig").Parser;
    // TODO: scan tokens
    const source = 
        \\ 1 + 2 + 3;
        ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var scanner = Scanner.new(source, allocator);
    // move forward
    const tokens = try scanner.scanTokens();

    var parser = Parser.new(tokens, allocator);
    var statements = try parser.parse();
    var interpreter = try Interpreter.init(allocator);

    while (statements.items.len > 0) {
        const statement = statements.orderedRemove(0);
        interpreter.execute(statement) catch |runtime_error| {
            if (runtime_error == RuntimeError.AllocError) {
                // Do nothing upon allocation error
                err.runtime_error_msg(null, "allocation error at runtime", runtime_error, allocator) catch {};
            }
        };
    }     

}

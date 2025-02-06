const std = @import("std");

const ArrayList = std.ArrayList;

const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const Object= @import("token/object.zig").Object;
const Tag = @import("token/object.zig").Tag;
const FunctionType = @import("token/object.zig").FunctionType;

const e= @import("expr.zig");
const ExprVisitor = @import("expr.zig").Visitor;

const s = @import("stmt.zig");
const StmtVisitor = @import("stmt.zig").Visitor;

const Function = @import("function.zig").Function;
const Callable = @import("callable.zig").Callable;

const Environment = @import("environment.zig").Environment;

const err = @import("error.zig");
const RuntimeError = err.RuntimeError;
const AllocationError = err.AllocationError;
const FunctionError = err.FunctionError;


// TODO: rethink this struct and whether I want the `Visitor`s as fields
// TODO: Interpreter defines RuntimeError land
pub const Interpreter = struct {
    const Self = @This();
    const T: type = RuntimeError!Object;
    const stmt_T: type = RuntimeError!void;

    allocator: std.mem.Allocator,
    environment: *Environment,
    globals: *Environment,

    // TODO: note the interpreter owns all subsequent types and thus the responsibility of deallocating heap memory should be with respect to the interpreter
    pub fn init(allocator: std.mem.Allocator) RuntimeError!Self {
        const env_ref = allocator.create(Environment) catch return err.outOfMemory();
        env_ref.* = Environment.init(allocator, null);
        const temp = .{ 
            .environment= env_ref,
            .globals = env_ref,
            .allocator=allocator
        };

        const Clock = struct {
            const S = @This();

            pub fn arity(self: *S) usize {
                _ = self;
                return 0;
            }

            pub fn call(self: *S, interpreter: *Interpreter, arguments: ArrayList(Object), alloc: std.mem.Allocator) FunctionError!Object {
                _ = alloc;
                _ = self;
                _ = interpreter;
                _ = arguments;
                std.debug.print("{d}\n", .{@divFloor(std.time.milliTimestamp(), 1000)});
                return Object{.Nil = null};
            }

            pub fn toString(self: *S, alloc: std.mem.Allocator) AllocationError![]const u8{
                _ = self;
                _ = alloc;
                return "<native fn>";
            }

            pub fn initCallable(self: *S) Callable() {
                return Callable().init(self);
            }
        };

        var clock = Clock{};
        const fntype_ref = allocator.create(FunctionType) catch return err.outOfMemory();
        fntype_ref.* = FunctionType{ .Native = clock.initCallable()};

        try temp.globals.define(
            "clock", 
            Object{.Function = fntype_ref}, 
            allocator);

        return temp;
    }

    pub fn interpret(self: *Self, statements: *ArrayList(s.Stmt)) RuntimeError!void {
        while (statements.items.len > 0) {
            const statement = statements.orderedRemove(0);
            try self.execute(statement);
            // DEBUG: environments
            // self.environment.print(self.allocator) catch return err.outOfMemory();
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
        const fntype_ref = self.allocator.create(FunctionType) catch return err.outOfMemory();
        const fn_ref = self.allocator.create(Function) catch return err.outOfMemory();
        fn_ref.* = Function.init(stmt);
        fntype_ref.*.Declared = fn_ref;
        // const callable_ref = self.allocator.create(Callable()) catch return RuntimeError.AllocError;
       
        try self.environment.define(stmt.name.lexeme, Object{.Function=fntype_ref}, self.allocator);
    }

    pub fn visitBreakStmt(self: *Self, stmt: s.Break) stmt_T {
        stmt.associated_condition.* = (try e.Literal.new(Object{.Bool = false}, self.allocator)).*;
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
        var enclosed_environment = self.allocator.create(Environment) catch return err.outOfMemory();
        enclosed_environment = self.environment;

        const block_environment = Environment.init(
            self.allocator, 
           enclosed_environment 
        );
        
        // TODO: rethink messaging for block
        _ = try self.executeBlock(
            stmt.statements, 
            block_environment
        );
    }

    pub fn executeBlock(self: *Self, statements: ArrayList(s.Stmt), environment: Environment) RuntimeError!void { 
        const parent_environment = self.environment; 
        self.environment.* = environment;
        defer self.environment = parent_environment; 
        errdefer self.environment = parent_environment;

        for (statements.items) |stmt| {
            // TODO: issue, if we execute a break, how do we jump out?
            switch (stmt) {
                .@"break" => {
                    try self.execute(stmt);
                    break;
                },
                else => {
                    try self.execute(stmt);
                }
            }
        }
    }

    pub fn visitExpressionStmt(self: *Self, stmt: s.Expression) stmt_T {
        const value = self.evaluate(stmt.expression) catch |runtime_err| return runtime_err;
        switch (stmt.expression.*) {
            .assign => {},
            else => {
                if (value != .Nil) {
                    const literal = try value.toString(self.allocator);
                    std.debug.print("{s}\n", .{literal});
                }
            }
        }
    }

    pub fn visitPrintStmt(self: *Self, stmt: s.Print) stmt_T {
        const value = try self.evaluate(stmt.expression);
        std.debug.print("{s}\n", .{try value.toString(self.allocator)});
    }

    pub fn visitVarStmt(self: *Self, stmt: s.Var) stmt_T {
        const value = self.allocator.create(Object) catch return err.outOfMemory();
        if (stmt.initializer) |checked_initializer| {
            // Instantiation
            value.* = try self.evaluate(checked_initializer);
            try self.environment.define(stmt.name.lexeme, value.*, self.allocator);
        } else {
            // Declaration
            try self.environment.define(stmt.name.lexeme, null, self.allocator);
        }
    }

    // visitorExpr logic
    pub fn visitCallExpr(self: *Self, expr: e.Call) T {
        const callee = try self.evaluate(expr.callee);
        var arguments = ArrayList(Object).init(self.allocator);
        for (expr.arguments.items) |arg| {
            arguments.append(try self.evaluate(arg)) catch return err.outOfMemory();
        }

        var funcType = switch (callee) {
            .Function => |funcType| funcType.*,
            else => {
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "calling undeclared function", 
                    .{}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.paren.line, err_msg, RuntimeError.UndeclaredObject, self.allocator);
            },
        };

        if (arguments.items.len != funcType.arity()) {
            const err_msg = std.fmt.allocPrint(
                self.allocator, 
                "expected {d} arguments, got {d}", 
                .{arguments.items.len, funcType.arity()}
            ) catch return err.outOfMemory();
            return err.errorMessage(RuntimeError, expr.paren.line, err_msg, RuntimeError.TooManyArguments, self.allocator);
             
        }
        return try funcType.call(self, arguments, self.allocator);

    }

    pub fn visitAssignExpr(self: *Self, expr: e.Assign) T {
        const value = try self.evaluate(expr.value);
        self.environment.assign(expr.name, value, self.allocator) catch return err.outOfMemory();
        return value;
    }

    pub fn visitVarExpr(self: *Self, expr: e.Var) T {
        const variable = self.environment.get(expr.name.lexeme, self.allocator) catch return err.outOfMemory(); // map to Runtime error
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
                if (!left.sameTags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Number = left.Number - right.Number};
            },
            TokenType.SLASH => {
                if (!left.sameTags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Number = left.Number / right.Number};

            },
            TokenType.STAR => {
                if (!left.sameTags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Number = left.Number * right.Number};
            },
            TokenType.PLUS => {
                if (!left.sameTags(right, Tag.String) and !left.sameTags(right, Tag.Number)) return RuntimeError.OperandError;
                if (left.checkTag(Tag.String)) {
                    const new_string = std.fmt.allocPrint(
                        self.allocator, 
                        "{s}{s}", 
                        .{left.String, right.String}
                    ) catch return err.outOfMemory();
                    return Object{ .String = new_string};
                } else {
                    return Object{ .Number = left.Number + right.Number};
                }
            },
            TokenType.GREATER => {
                if (!left.sameTags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Bool = left.Number > right.Number};
            },
            TokenType.GREATER_EQUAL => {
                if (!left.sameTags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Bool = left.Number >= right.Number};
            },
            TokenType.LESS => {
                if (!left.sameTags(right, Tag.Number)) return RuntimeError.OperandError;
                return Object{ .Bool = left.Number < right.Number};
            },
            TokenType.LESS_EQUAL => {
                if (!left.sameTags(right, Tag.Number)) return RuntimeError.OperandError;
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
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "operator {s} is not a binary operator", 
                    .{expr.operator.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.operator.line, err_msg, runtime_err, self.allocator);
            },
            .OperandError => {
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "operands {any} and {any} are not compatible with the operator {s}", 
                    // TODO: double-check impl
                    .{left, right, expr.operator.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.operator.line, err_msg, runtime_err, self.allocator);
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
                if (!right.checkTag(Tag.Number)) return RuntimeError.OperandError;
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
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.operator.line, err_msg, runtime_err, self.allocator);
            },
            .OperandError => {
                const err_msg = std.fmt.allocPrint(
                    self.allocator, 
                    "operand {any} is not compatible with operator {s}", 
                    .{right, expr.operator.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.operator.line, err_msg, runtime_err, self.allocator);
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
                err.errorMessage(RuntimeError, null, "allocation error at runtime", runtime_error, allocator) catch {};
            }
        };
    }     

}

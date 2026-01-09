const std = @import("std");

const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const StringHashMap = std.StringHashMap;

const Token = @import("primitives/token.zig").Token;
const TokenType = @import("primitives/token_type.zig").TokenType;
const Object= @import("primitives/object.zig").Object;
const Tag = @import("primitives/object.zig").Tag;
const e= @import("primitives/expr.zig");
const ExprVisitor = e.Visitor;
const s = @import("primitives/stmt.zig");
const StmtVisitor = s.Visitor;

const Callable = @import("primitives/callable/callable.zig").Callable;
const native = @import("primitives/callable/native.zig");
const Function = @import("primitives/callable/function.zig").Function;
const Class = @import("primitives/callable/class.zig").Class;
const Instance = @import("primitives/callable/instance.zig").Instance;

const Environment = @import("environment.zig").Environment;

const err = @import("error.zig");
const RuntimeError = err.RuntimeError;
const CompileError = err.CompileError;
const AllocationError = err.AllocationError;
const FunctionError = err.FunctionError;

// TODO: rethink this struct and whether I want the `Visitor`s as fields
// TODO: Interpreter defines RuntimeError land
pub const Interpreter = struct {
    const Self = @This();
    const T: type = RuntimeError!Object;
    const stmt_T: type = RuntimeError!?Object;

    allocator: std.mem.Allocator,
    environment: *Environment,
    globals: *Environment,
    returnValue: ?Object,
    // TODO: figre out a way to store in an array instead of a hashmap
    // resolver should return the appropriate index based on `addr`
    locals: AutoHashMap(usize, usize),

    // TODO: note the interpreter owns all subsequent types and thus the responsibility of deallocating heap memory should be with respect to the interpreter
    pub fn init(allocator: std.mem.Allocator) RuntimeError!Self {
        const env_ref = allocator.create(Environment) catch return err.outOfMemory();
        // NOTE: parent environment
        env_ref.* = Environment.init(allocator, null);
        const temp: Self = .{ 
            .environment= env_ref,
            .globals = env_ref,
            .allocator = allocator,
            .returnValue = null,
            .locals = AutoHashMap(usize, usize).init(allocator),
        };

        var clock = native.Clock{};
        const clock_ref = allocator.create(Callable) catch return err.outOfMemory();
        clock_ref.* = Callable{ .Native = clock.initCallable()};
        try temp.globals.define(
            "clock", 
            Object{.Function = clock_ref}, 
            allocator);

        var panic = native.Panic{};
        const panic_ref = allocator.create(Callable) catch return err.outOfMemory();
        panic_ref.* = Callable{ .Native = panic.initCallable()};
        try temp.globals.define(
            "panic", 
            Object{.Function = panic_ref}, 
            allocator);

        return temp;
    }

    pub fn interpret(self: *Self, statements: *ArrayList(*s.Stmt)) RuntimeError!void {
        while (statements.items.len > 0) {
            const statement = statements.orderedRemove(0);
            _ = try self.execute(statement);
        }     
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    // KEY: this is a key step where we populate solutions in an AST prior to interpreting
    pub fn resolve(self: *Self, addr: usize, depth: usize) CompileError!void {
        try self.locals.put(addr, depth);
    }

    // private methods
    fn evaluate(self: *Self, expr: *e.Expr) T {
        const visitor = self.initExprVisitor();
        // DEBUGGING
        // expr.activeField();
        return expr.accept(T, visitor);
    }


    fn execute(self: *Self, stmt: *s.Stmt) stmt_T {
        const visitor = self.initStmtVisitor();
        // DEBUGGING
        // stmt.activeField();
        return try stmt.accept(stmt_T, visitor);
    }

    fn initExprVisitor(self: *Self) ExprVisitor(T) {
        return ExprVisitor(T).init(self);
    }

    fn initStmtVisitor(self: *Self) StmtVisitor(stmt_T) {
        return StmtVisitor(stmt_T).init(self);
    }

    // visitorStmt logic
    pub fn visitClassStmt(self: *Self, stmt: s.Class) stmt_T {
        var superclass: ?Object = null;
        // NOTE: I feel like there is a better way to write this
        if (stmt.superclass) |parsed_superclass| {
            superclass = try self.evaluate(parsed_superclass);
            switch (superclass.?) {
                .Class => {
                },
                else => {
                    return err.errorMessage(RuntimeError, stmt.name.line, "The superclass must be a class", RuntimeError.InvalidFieldAccess, self.allocator);
                },
            }
        }

        try self.environment.define(stmt.name.lexeme, null, self.allocator);

        if (stmt.superclass) |_| {
            const environment_ref = try self.allocator.create(Environment);
            environment_ref.* = Environment.init(self.allocator, self.environment);
            self.environment = environment_ref;
            try self.environment.define("super", superclass, self.allocator);
        }

        var methods = StringHashMap(Object).init(self.allocator);
        for (stmt.methods.items) |maybe_method| {
            switch (maybe_method.*) {
                .function => |method| {
                    const callable_ref = try self.allocator.create(Callable);
                    callable_ref.* = Callable { .Declared = try Function.init(method, self.environment, std.mem.eql(u8, method.name.lexeme, "init"))};
                    try methods.put( method.name.lexeme, Object{ .@"Function" = callable_ref });
                },
                else => unreachable
            }
        }
        const class_type_ref = self.allocator.create(Callable) catch return err.outOfMemory();
        class_type_ref.*.Class = Class.init(stmt.name.lexeme, superclass, methods);

        if (superclass) |_| {
            self.environment = self.environment.enclosing.?;
        }

        try self.environment.assign(stmt.name, Object{ .Class = class_type_ref }, self.allocator);
        return null;
    }

    pub fn visitFunctionStmt(self: *Self, stmt: s.Function) stmt_T {
        const fntype_ref = self.allocator.create(Callable) catch return err.outOfMemory();
        fntype_ref.* = Callable { .Declared = try Function.init(stmt, self.environment, false) };
        try self.environment.define(stmt.name.lexeme, Object{.Function=fntype_ref}, self.allocator);
        return null;
    }

    pub fn visitBreakStmt(self: *Self, stmt: s.Break) stmt_T {
        stmt.associated_condition.* = (try e.Literal.new(Object{.Bool = false}, self.allocator)).*;
        return null;
    }

    pub fn visitWhileStmt(self: *Self, stmt: s.While) stmt_T {
        while ((try self.evaluate(stmt.condition)).isTruthy()) {
            const value = try self.execute(stmt.body);
            if (value != null) return value;
        }
        return null;
    }

    pub fn visitReturnStmt(self: *Self, stmt: s.Return) stmt_T {
        var value: ?Object = null;  
        if (stmt.value != null) value = try self.evaluate(stmt.value.?);
        return value;
    }

    pub fn visitIfStmt(self: *Self, stmt: s.If) stmt_T {
        if ((try self.evaluate(stmt.condition)).isTruthy()) {
            return try self.execute(stmt.then_branch);
        } else if (stmt.else_branch) |else_branch| {
            return try self.execute(else_branch);
        }     
        return null;
    }

    pub fn visitBlockStmt(self: *Self, stmt: s.Block) stmt_T {
         const block_environment = try self.allocator.create(Environment);
         block_environment.* = Environment.init(
            self.allocator, 
           self.environment 
        );
        
        return try self.executeBlock(
            stmt.statements, 
            block_environment
        );
    }

    pub fn executeBlock(self: *Self, statements: ArrayList(*s.Stmt), environment: *Environment) stmt_T { 
        // NOTE: need to recover the parent environment
        const parent_environment = self.environment; 
        self.environment = environment;
        defer self.environment = parent_environment; 
        errdefer self.environment = parent_environment;

        for (statements.items) |stmt| {
            // TODO: issue, if we execute a break, how do we jump out?
            switch (stmt.*) {
                .@"break" => {
                    _ = try self.execute(stmt);
                    break;
                },
                else => {
                    const value = try self.execute(stmt);
                    if (value != null) return value;
                }
            }
        }
        return null;
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
        return null;
    }

    pub fn visitPrintStmt(self: *Self, stmt: s.Print) stmt_T {
        const value = try self.evaluate(stmt.expression);
        std.debug.print("{s}\n", .{try value.toString(self.allocator)});
        return null;
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
        return null;
    }

    // visitorExpr logic
    pub fn visitSuperExpr(self: *Self, expr: e.Super, addr: usize) T {
        const distance = self.locals.get(addr).?;
        const superclass = try self.environment.getAt(distance, "super", self.allocator);
        var instance_ref: ?*Instance = null;
        switch (try self.environment.getAt(distance-1, "this", self.allocator)) {
            .Instance => |callable| { 
                switch (callable.*) {
                    .Instance => |instance| {
                        instance_ref = instance;
                    },
                    else => unreachable
                }
            },
            else => unreachable
        }
        var maybe_method: ?Object = null;
        switch (superclass) {
            .Class => |callable| {
                switch (callable.*) {
                    .Class => |unwrap_class| {
                        maybe_method = unwrap_class.findMethod(expr.method.lexeme);
                    },
                    else => unreachable
                }
            },
            else => unreachable
        }

        if (maybe_method) |method| {
            return try method.bind(instance_ref.?, self.allocator);
        } 

        const error_message = std.fmt.allocPrint(
            self.allocator, 
            "Undefined property {s}.", 
            .{expr.method.lexeme}
        ) catch return err.outOfMemory();
        return err.errorMessage(RuntimeError, expr.keyword.line, error_message, RuntimeError.UndefinedProperty, self.allocator);
    }

    pub fn visitSetExpr(self: *Self, expr: e.Set) T {
        const object = try self.evaluate(expr.object);
        const value = try self.evaluate(expr.value);
        switch (object) {
            .Instance => |callable| {
                // NOTE: the Instance Object is only defined with the Callable.Instance field active
                switch (callable.*) {
                    .Instance => |instance| {
                        try instance.set(expr.name, value);
                        return value;
                    },
                    else => unreachable 
                }
            },
            else => {  
                const error_message = std.fmt.allocPrint(
                    self.allocator, 
                    "{s} is not an instance of a class. Only instances have fields.", 
                    .{expr.name.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.name.line, error_message, RuntimeError.InvalidFieldAccess, self.allocator);
            }
        }
        return expr.value;
    }

    pub fn visitThisExpr(self: *Self, expr: e.This, addr: usize) T {
        return self.lookUpVar(expr.keyword, addr);
    }

    pub fn visitGetExpr(self: *Self, expr: e.Get) T {
        const object = try self.evaluate(expr.object);
        switch (object) {
            .Instance => |callable| {
                // NOTE: the Instance Object is only defined with the Callable.Instance field active
                switch (callable.*) {
                    .Instance => |instance| return instance.get(expr.name, self.allocator),
                    else => unreachable 
                }
            },
            else => {  
                const error_message = std.fmt.allocPrint(
                    self.allocator, 
                    "{s} is not an instance of a class. Only instances have properties.", 
                    .{expr.name.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.name.line, error_message, RuntimeError.InvalidPropertyAccess, self.allocator);
            }
        }
        return expr.value;
    }

    pub fn visitCallExpr(self: *Self, expr: e.Call) T {
        const callee = try self.evaluate(expr.callee);

        var arguments: ArrayList(Object) = .empty;
        for (expr.arguments.items) |arg| {
            arguments.append(self.allocator, try self.evaluate(arg)) catch return err.outOfMemory();
        }

        var callable = switch (callee) {
            .Function => |callable| callable.*,
            .Class => |callable| callable.*,
            .Instance => |callable| callable.*,
            else => {
                const error_message = std.fmt.allocPrint(
                    self.allocator, 
                    "calling undeclared function", 
                    .{}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.paren.line, error_message, RuntimeError.UndeclaredObject, self.allocator);
            },
        };

        if (arguments.items.len != callable.arity()) {
            const error_message = std.fmt.allocPrint(
                self.allocator, 
                "expected {d} arguments, got {d}", 
                .{arguments.items.len, callable.arity()}
            ) catch return err.outOfMemory();
            return err.errorMessage(RuntimeError, expr.paren.line, error_message, RuntimeError.TooManyArguments, self.allocator);
        }

        return try callable.call(self, arguments, self.allocator);
    }

    pub fn visitAnonymousExpr(self: *Self, expr: e.Anonymous) T {
        const fntype_ref = self.allocator.create(Callable) catch return err.outOfMemory();
        fntype_ref.* = Callable { .Declared = try Function.init(expr.function, self.environment, false) };
        return Object{.Function=fntype_ref};
    }

    pub fn visitAssignExpr(self: *Self, expr: e.Assign, addr: usize) T {
        const value = try self.evaluate(expr.value);
        const distance = self.locals.get(addr);
        if (distance != null) {
            try self.environment.assignAt(distance.?, expr.name, value, self.allocator);
        } else {
            try self.globals.assign(expr.name, value, self.allocator);
        }
        return value;
        // self.environment.assign(expr.name, value, self.allocator) catch return err.outOfMemory();
    }

    pub fn visitVarExpr(self: *Self, expr: e.Var, addr: usize) T {
        return self.lookUpVar(expr.name, addr);
    }

    fn lookUpVar(self: *Self, name: Token, addr: usize) T {
        const distance = self.locals.get(addr);
        if (distance != null) {
            return self.environment.getAt(distance.?, name.lexeme, self.allocator);
        } else {
            return self.globals.get(name.lexeme, self.allocator);
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
                const error_message = std.fmt.allocPrint(
                    self.allocator, 
                    "operator {s} is not a binary operator", 
                    .{expr.operator.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.operator.line, error_message, runtime_err, self.allocator);
            },
            .OperandError => {
                const error_message = std.fmt.allocPrint(
                    self.allocator, 
                    "operands {any} and {any} are not compatible with the operator {s}", 
                    // TODO: double-check impl
                    .{left, right, expr.operator.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.operator.line, error_message, runtime_err, self.allocator);
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
                const error_message = std.fmt.allocPrint(
                    self.allocator, 
                    "operator {s} is not a unary operator", 
                    .{expr.operator.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.operator.line, error_message, runtime_err, self.allocator);
            },
            .OperandError => {
                const error_message = std.fmt.allocPrint(
                    self.allocator, 
                    "operand {any} is not compatible with operator {s}", 
                    .{right, expr.operator.lexeme}
                ) catch return err.outOfMemory();
                return err.errorMessage(RuntimeError, expr.operator.line, error_message, runtime_err, self.allocator);
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

test "recursion-test" {
    const source = 
        \\ 
        ;
    const Scanner = @import("scanner.zig");
    const Parser = @import("parser.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var scanner = Scanner.new(source, allocator);
    const tokens = try scanner.scanTokens();

    var parser = Parser.init(tokens, allocator);

    var statements  = try parser.parse(); 
    var interpreter = try Interpreter.init(allocator);
    _ = try interpreter.interpret(&statements);
}

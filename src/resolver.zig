const std = @import("std");

const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const Object= @import("token/object.zig").Object;
const Tag = @import("token/object.zig").Tag;
const FunctionType = @import("token/object.zig").FunctionType;

const e= @import("expr.zig");
const ExprVisitor = @import("expr.zig").Visitor;

const s = @import("stmt.zig");
const StmtVisitor = @import("stmt.zig").Visitor;

const native = @import("native.zig");
const Function = @import("function.zig").Function;
const Callable = @import("callable.zig").Callable;

const Environment = @import("environment.zig").Environment;
const Interpreter = @import("interpreter.zig").Interpreter;

const err = @import("error.zig");
// const ResolutionError = err.ResolutionError;
const AllocationError = err.AllocationError;
const FunctionError = err.FunctionError;
const ResolutionError = err.ResolutionError;

pub const Resolver = struct {
    const Self = @This();
    const T: type = ResolutionError!void;

    interpreter: *Interpreter,
    scopes: ArrayList(*StringHashMap(bool)),
    allocator: std.mem.Allocator,

    pub fn init(interpreter: *Interpreter, allocator: std.mem.Allocator) Resolver {
        return Resolver{ 
            .interpreter = interpreter,
            .scopes = ArrayList(*StringHashMap(bool)).init(allocator),
            .allocator = allocator,
        };
    }

    fn initExprVisitor(self: *Self) ExprVisitor(T) {
        return ExprVisitor(T).init(self);
    }

    fn initStmtVisitor(self: *Self) StmtVisitor(T) {
        return StmtVisitor(T).init(self);
    }

    pub fn resolveStatement(self: *Self, stmt: *s.Stmt) T {
        const visitor = self.initStmtVisitor();
        return try stmt.accept(T, visitor);
    }

    fn resolveExpr(self: *Self, expr: *e.Expr) T {
        const visitor = self.initExprVisitor();
        return expr.accept(T, visitor);
    }

    fn beginScope(self: *Self) AllocationError!void {
        const map = self.allocator.create(StringHashMap(bool)) catch return err.outOfMemory();
        map.* = StringHashMap(bool).init(self.allocator);
        try self.scopes.append(map);
    }

    fn endScope(self: *Self) void {
        _ = self.scopes.pop();
    }

    fn declare(self: *Self, name: Token) AllocationError!void {
        if (self.scopes.items.len == 0) return;
        self.scopes.getLast().put(name.lexeme, false) catch return err.outOfMemory();
    }

    fn define(self: *Self, name: Token) AllocationError!void {
        if (self.scopes.items.len == 0) return;
        self.scopes.getLast().put(name.lexeme, true) catch return err.outOfMemory();
    }

    // Implement Statements
    pub fn visitBlockStmt(self: *Self, stmt: s.Block) T {
        try self.beginScope();
        for (stmt.statements.items) |statement| {
            try self.resolveStatement(statement);
        }
        self.endScope();
    }

    pub fn visitVarStmt(self: *Self, stmt: s.Var) T {
        try self.declare(stmt.name);
        if (stmt.initializer != null) {
            try self.resolveExpr(stmt.initializer.?);
        }
        try self.define(stmt.name);
    }

    pub fn visitFunctionStmt(self: *Self, stmt: s.Function) T {
        try self.declare(stmt.name);
        try self.define(stmt.name);
        try self.resolveFunction(stmt);
    }

    fn resolveFunction(self: *Self, function: s.Function) T {
        try self.beginScope();
        for (function.params.items) |param| {
            try self.declare(param);
            try self.define(param);
        }
        for (function.body.items) |stmt| try self.resolveStatement(stmt);
        self.endScope();
    }

    pub fn visitExpressionStmt(self: *Self, stmt: s.Expression) T {
        try self.resolveExpr(stmt.expression);
    }

    pub fn visitIfStmt(self: *Self, stmt: s.If) T {
        try self.resolveExpr(stmt.condition);
        try self.resolveStatement(stmt.then_branch);
        if (stmt.else_branch != null) try self.resolveStatement(stmt.else_branch.?);
    }

    pub fn visitPrintStmt(self: *Self, stmt: s.Print) T {
        try self.resolveExpr(stmt.expression);
    }

    pub fn visitReturnStmt(self: *Self, stmt: s.Return) T {
        if (stmt.value != null) {
            try self.resolveExpr(stmt.value.?);
        }
    }

    pub fn visitBreakStmt(self: *Self, stmt: s.Break) T {
        _ = self;
        _ = stmt;
    }

    pub fn visitWhileStmt(self: *Self, stmt: s.While) T {
        try self.resolveExpr(stmt.condition);
        try self.resolveStatement(stmt.body);
    }

    // Implement Expressions
    pub fn visitVarExpr(self: *Self, expr: e.Var, addr: usize) T {
        const scopes_size = self.scopes.items.len;
        if (scopes_size > 0) {
            const varResolved = self.scopes.getLast().get(expr.name.lexeme);
            if (varResolved != null and varResolved.? == false) return err.errorMessage(ResolutionError, expr.name.line, "Can't read local variable in its own initializer.", ResolutionError.VariableShadow, self.allocator);
        }
        try self.resolveLocal(addr, expr.name);
    }

    // Look in current scope and above
    fn resolveLocal(self: *Self, addr: usize, name: Token) T {
        var i = self.scopes.items.len;
        while (i > 0) {
            i -= 1; 
            const scope = self.scopes.items[i];
            if (scope.get(name.lexeme) != null) {
                try self.interpreter.resolve(addr, self.scopes.items.len - 1 - i);
            }
        }
    }

    pub fn visitAssignExpr(self: *Self, expr: e.Assign, addr: usize) T {
        try self.resolveExpr(expr.value);
        try self.resolveLocal(addr, expr.name);
    }

    pub fn visitBinaryExpr(self: *Self, expr: e.Binary) T {
        try self.resolveExpr(expr.left);
        try self.resolveExpr(expr.right);
    }

    pub fn visitCallExpr(self: *Self, expr: e.Call) T {
        try self.resolveExpr(expr.callee);
        for (expr.arguments.items) |argument| try self.resolveExpr(argument);
    }

    pub fn visitGroupingExpr(self: *Self, expr: e.Grouping) T {
        try self.resolveExpr(expr.expression);
    }

    pub fn visitLiteralExpr(self: *Self, expr: e.Literal) T {
        _ = self;
        _ = expr;
    }

    pub fn visitLogicalExpr(self: *Self, expr: e.Logical) T {
        try self.resolveExpr(expr.left);
        try self.resolveExpr(expr.right);
    }

    pub fn visitUnaryExpr(self: *Self, expr: e.Unary) T {
        try self.resolveExpr(expr.right);
    }

    pub fn visitAnonymousExpr(self: *Self, expr: e.Anonymous) T {
        // TODO: double check logic
        try self.beginScope();
        for (expr.function.params.items) |param| {
            try self.declare(param);
            try self.define(param);
        }
        for (expr.function.body.items) |stmt| try self.resolveStatement(stmt);
        self.endScope();
    }
};


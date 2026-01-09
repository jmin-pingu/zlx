const std = @import("std");

const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Token = @import("primitives/token.zig").Token;
const Object= @import("primitives/object.zig").Object;
const e= @import("primitives/expr.zig");
const ExprVisitor = e.Visitor;
const s = @import("primitives/stmt.zig");
const StmtVisitor = s.Visitor;

const Interpreter = @import("interpreter.zig").Interpreter;

const err = @import("error.zig");
const AllocationError = err.AllocationError;
const FunctionError = err.FunctionError;
const CompileError = err.CompileError;

const FunctionScopeType = union(enum) {
    None,
    Function,
    Method,
    Initializer,
};

const LoopScopeType = union(enum) {
    None,
    Loop,
};

const ClassScopeType = union(enum) {
    None,
    Class,
    Subclass,
};

pub const Resolver = struct {
    const Self = @This();
    const T: type = CompileError!void;

    interpreter: *Interpreter,
    scopes: ArrayList(*StringHashMap(bool)),
    allocator: std.mem.Allocator,
    currentFunction: FunctionScopeType = FunctionScopeType.None,
    currentLoop: LoopScopeType = LoopScopeType.None,
    currentClass: ClassScopeType = ClassScopeType.None,

    pub fn init(interpreter: *Interpreter, allocator: std.mem.Allocator) Resolver {
        return Resolver{ 
            .interpreter = interpreter,
            .scopes = .empty,
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
        try self.scopes.append(self.allocator, map);
    }

    fn endScope(self: *Self) void {
        _ = self.scopes.pop();
    }

    fn declare(self: *Self, name: Token) CompileError!void {
        if (self.scopes.items.len == 0) return;
        if (self.scopes.getLast().get(name.lexeme) != null) return err.errorMessage(CompileError, name.line, "Variable already declared in this scope", CompileError.RepeatVariableDeclaration, self.allocator);
        self.scopes.getLast().put(name.lexeme, false) catch return err.outOfMemory();
    }

    fn define(self: *Self, name: Token) AllocationError!void {
        if (self.scopes.items.len == 0) return;
        self.scopes.getLast().put(name.lexeme, true) catch return err.outOfMemory();
    }

    // Implement Statements
    pub fn visitDeclarationStmt(self: *Self, stmt: s.Declaration) T {
        try self.declare(stmt.name);
        try self.define(stmt.name);
    }

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
        try self.resolveFunction(stmt, FunctionScopeType.Function);
    }

    fn resolveFunction(self: *Self, function: s.Function, stype: FunctionScopeType) T {
        const enclosing_function = self.currentFunction;
        defer self.currentFunction = enclosing_function;
        self.currentFunction = stype;
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
        const enclosing_loop = self.currentLoop;
        defer self.currentLoop = enclosing_loop;
        self.currentLoop = LoopScopeType.Loop;

        try self.resolveExpr(stmt.condition);
        try self.resolveStatement(stmt.then_branch);
        if (stmt.else_branch != null) try self.resolveStatement(stmt.else_branch.?);
    }

    pub fn visitClassStmt(self: *Self, stmt: s.Class) T {
        const enclosing_class = self.currentClass;
        defer self.currentClass = enclosing_class;
        self.currentClass = ClassScopeType.Class;

        try self.declare(stmt.name);
        try self.define(stmt.name);
        
        if (stmt.superclass) |superclass| {
            self.currentClass = ClassScopeType.Subclass;
            if (std.mem.eql(u8, stmt.name.lexeme, superclass.@"var".name.lexeme)) return err.errorMessage(CompileError, stmt.name.line, "A class cannot inherit from itself.", CompileError.RecursiveInheritanceError, self.allocator);
 
            try self.resolveExpr(superclass);
            try self.beginScope();
            try self.scopes.getLast().put("super", true);
        }

        try self.beginScope();

        try self.scopes.getLast().put("this", true);

        for (stmt.methods.items) |method| {
            if (std.mem.eql(u8, stmt.name.lexeme, "init")) {
                try self.resolveFunction(method.function, FunctionScopeType.Initializer);
            } else {
                try self.resolveFunction(method.function, FunctionScopeType.Method);
            }
        }
        self.endScope();

        if (stmt.superclass) |_| { 
            self.endScope(); 
        }
    }

    pub fn visitPrintStmt(self: *Self, stmt: s.Print) T {
        try self.resolveExpr(stmt.expression);
    }

    pub fn visitReturnStmt(self: *Self, stmt: s.Return) T {
        if (self.currentFunction == FunctionScopeType.None) return err.errorMessage(CompileError, stmt.keyword.line, "Return not nested in function", CompileError.IncorrectReturnScope, self.allocator);

        if (stmt.value != null) {

            if (self.currentFunction == FunctionScopeType.Initializer) return err.errorMessage(CompileError, stmt.keyword.line, "Can't return a value from an initializer", CompileError.IncorrectInitScope, self.allocator);
            try self.resolveExpr(stmt.value.?);
        }
    }

    pub fn visitBreakStmt(self: *Self, stmt: s.Break) T {
        if (self.currentLoop == LoopScopeType.None) return err.errorMessage(CompileError, stmt.keyword.line, "Break not nested in loop", CompileError.IncorrectBreakScope, self.allocator);
    }

    pub fn visitWhileStmt(self: *Self, stmt: s.While) T {
        const enclosing_loop = self.currentLoop;
        defer self.currentLoop = enclosing_loop;
        self.currentLoop = LoopScopeType.Loop;
        try self.resolveExpr(stmt.condition);
        try self.resolveStatement(stmt.body);
    }

    // Implement Expressions
    pub fn visitSuperExpr(self: *Self, expr: e.Super, addr: usize) T {
        switch (self.currentClass) {
            .Class => return err.errorMessage(CompileError, expr.keyword.line, "Can't use 'super' in a class with no superclass", CompileError.IncorrectSuperScope, self.allocator),
            .None => return err.errorMessage(CompileError, expr.keyword.line, "Can't use 'super' outside of a class.", CompileError.IncorrectSuperScope, self.allocator),
            .Subclass => {},
        }
        try self.resolveLocal(addr, expr.keyword);
    }

    pub fn visitVarExpr(self: *Self, expr: e.Var, addr: usize) T {
        const scopes_size = self.scopes.items.len;
        if (scopes_size > 0) {
            const varResolved = self.scopes.getLast().get(expr.name.lexeme);
            if (varResolved != null and varResolved.? == false) return err.errorMessage(CompileError, expr.name.line, "Can't read local variable in its own initializer.", CompileError.VariableShadow, self.allocator);
        }         
        try self.resolveLocal(addr, expr.name);
    }

    // Look in current scope and above
    fn resolveLocal(self: *Self, addr: usize, name: Token) T {
        var i = self.scopes.items.len;
        // for each unique address, we can increment by 1
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

    pub fn visitThisExpr(self: *Self, expr: e.This, addr: usize) T {
        if (self.currentClass == ClassScopeType.None) {
            return err.errorMessage(CompileError, expr.keyword.line, "Can't use `this` outside of a class", CompileError.IncorrectThisScope, self.allocator);
        }
        try self.resolveLocal(addr, expr.keyword);
    }

    pub fn visitGetExpr(self: *Self, expr: e.Get) T {
        try self.resolveExpr(expr.object);
    }

    pub fn visitSetExpr(self: *Self, expr: e.Set) T {
        try self.resolveExpr(expr.value);
        try self.resolveExpr(expr.object);
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


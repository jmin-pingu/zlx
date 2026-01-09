const std = @import("std");

const ArrayList = std.ArrayList;

const Token = @import("primitives/token.zig").Token;
const TokenType = @import("primitives/token_type.zig").TokenType;
const Object = @import("primitives/object.zig").Object;
const expr = @import("primitives/expr.zig");
const Stmt = @import("primitives/stmt.zig").Stmt;
const s = @import("primitives/stmt.zig");

const err = @import("error.zig");
const Error = err.Error;
const VariableError = err.VariableError;
const FunctionError = err.FunctionError;
const ParseError = err.ParseError;

// TODO: Add more granular errors for ParserErrors
const equality_match = [2]TokenType{TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL};
const unary_match = [2]TokenType{TokenType.BANG, TokenType.MINUS};
const factor_match = [2]TokenType{TokenType.SLASH, TokenType.STAR};
const comparison_match = [4]TokenType{TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL};
const term_match = [2]TokenType{TokenType.MINUS, TokenType.PLUS};

// TODO: need to reason better about when to heap allocate
pub const Parser = struct {
    tokens: ArrayList(Token),
    current: u64 = 0,

    pub fn init(tokens: ArrayList(Token)) Parser {
        return Parser{ .tokens = tokens };
    }

    // TODO: double-check logic for freeing! Need a better understanding of memory
    // pub fn deinit(self: *Parser) void {
    //     allocator.free(self.tokens);
    // }
 
    pub fn parse(self: *Parser, allocator: std.mem.Allocator) ParseError!ArrayList(*Stmt) {
        var statements: ArrayList(*Stmt) = .empty;
        // TODO: do we need to dealloc the ArrayList(Stmt)
        while (!self.reachedEnd()) {
            statements.append(allocator, try self.declaration(allocator)) catch return err.outOfMemory();
        }
        return statements;
    }

    fn declaration(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        try {
            if (self.match(1, [1]TokenType{TokenType.CLASS})) return try self.classDeclaration(allocator);
            if (self.match(1, [1]TokenType{TokenType.FUN})) return try self.function("function", allocator);
            if (self.match(1, [1]TokenType{TokenType.VAR})) return try self.varDeclaration(allocator);
            return try self.statement(allocator);
        } catch {
            self.synchronize();
            return ParseError.SyntaxError;
        };
    }

    fn classDeclaration(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        const name = try self.consume(TokenType.IDENTIFIER, "Expect class name.");
        var superclass: ?*expr.Expr = null;
        if (self.match(1, [1]TokenType{TokenType.LESS})) {
            _ = try self.consume(TokenType.IDENTIFIER, "Expect superclass name.");
            superclass = expr.Var.new(self.previous(), allocator) catch return ParseError.SyntaxError;
        }         
        _ = try self.consume(TokenType.LEFT_BRACE, "Expect '{' before class body.");

        var methods: ArrayList(*s.Stmt) = .empty;
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) {
            try methods.append(allocator, try self.function("method", allocator));
        }

        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.");
 
        return s.Class.new(name, superclass, methods, allocator);
    }

    fn function(self: *Parser, kind: []const u8, allocator: std.mem.Allocator) ParseError!*Stmt {
        var err_msg = std.fmt.allocPrint(allocator, "Expect {s} name", .{kind}) catch return err.outOfMemory();
        const name = try self.consume(TokenType.IDENTIFIER, err_msg);
        err_msg = std.fmt.allocPrint(allocator, "Expect '(' after {s} name", .{kind}) catch return err.outOfMemory();
        _ = try self.consume(TokenType.LEFT_PAREN, "");

        var parameters: ArrayList(Token) = .empty;
        while (!self.check(TokenType.RIGHT_PAREN)) {
            if (parameters.items.len >= 255) return err.errorMessage(ParseError, self.peek().line, "Can't have more than 255 parameters", ParseError.TooManyArguments, allocator);

            parameters.append(allocator, try self.consume(TokenType.IDENTIFIER, "Expect parameter name")) catch return err.outOfMemory();
            _ = self.match(1, [1]TokenType{TokenType.COMMA});
        }
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters");
        err_msg = std.fmt.allocPrint(allocator, "Expect '{{' before {s} body", .{kind}) catch return err.outOfMemory();
        _ = try self.consume(TokenType.LEFT_BRACE, err_msg);
        const body = try self.block(allocator);
        return s.Function.new(name, parameters, body, allocator);
    }


    fn varDeclaration(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        const name = try self.consume(TokenType.IDENTIFIER, "Expect variable name");
        var initializer: ?*expr.Expr = null; 

        if (self.match(1, [1]TokenType{TokenType.EQUAL})) initializer = try self.expression(allocator);
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration");
        return s.Var.new(name, initializer, allocator);
    }

    fn statement(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        if (self.match(1, [1]TokenType{TokenType.BREAK})) return err.errorMessage(ParseError, self.peek().line, "break not nested within control flow", ParseError.SyntaxError, allocator);
        if (self.match(1, [1]TokenType{TokenType.FOR})) return self.forStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.IF})) return self.ifStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.PRINT})) return self.printStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.RETURN})) return self.returnStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.WHILE})) return self.whileStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.LEFT_BRACE})) return s.Block.new(try self.block(allocator), allocator);
        return self.expressionStatement(allocator);
    }

    fn forStatement(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'");
        var initializer: ?*Stmt = undefined;
        if (self.match(1, [1]TokenType{TokenType.SEMICOLON})) {
            initializer = null;
        } else if (self.match(1, [1]TokenType{TokenType.VAR})) {
            initializer = try self.varDeclaration(allocator);
        } else {
            initializer = try self.expressionStatement(allocator);
        }

        var condition: ?*expr.Expr = null;
        if (!self.check(TokenType.SEMICOLON)) {
            condition = try self.expression(allocator);
        } else {
            condition = try expr.Literal.new(Object{.Bool = true}, allocator);
        }
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after loop condition");

        var increment: ?*expr.Expr = null;
        if (!self.check(TokenType.RIGHT_PAREN)) {
            increment = try self.expression(allocator);
        }         
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition");

        var body = try self.breakStatement(condition.?, allocator);

        if (increment != null) {
            var statements: ArrayList(*Stmt) = .empty;
            statements.append(allocator, body) catch return err.outOfMemory();
            statements.append(allocator, try s.Expression.new(increment.?, allocator)) catch return err.outOfMemory();
            body = try s.Block.new(statements, allocator);
        }

        body = try s.While.new(condition.?, body, allocator);

        if (initializer != null) {
            var statements: ArrayList(*Stmt) = .empty;
            statements.append(allocator, initializer.?) catch return err.outOfMemory();
            statements.append(allocator, body) catch return err.outOfMemory();
            body = try s.Block.new(statements, allocator);
        }
        return body;
    }

    fn returnStatement(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        const keyword  = self.previous();
        var value: ?*expr.Expr = null;
        if (!self.check(TokenType.SEMICOLON)) {
            value = try self.expression(allocator);
        }
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after 'return'");
        return s.Return.new(keyword, value, allocator);
    }

    fn whileStatement(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'");
        const condition = try self.expression(allocator);
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition");
        const body = try self.breakStatement(condition, allocator);
        return try s.While.new(condition, body, allocator);
    }

    fn ifStatement(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'");
        const condition = try self.expression(allocator);

        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition");
        const then_branch = try self.statement(allocator);

        var else_branch: ?*Stmt = null;
        if (self.match(1, [1]TokenType{TokenType.ELSE})) {
            else_branch = try self.statement(allocator);
        }
        return try s.If.new(condition, then_branch, else_branch, allocator);
    }


    fn breakStatement(self: *Parser, break_condition: *expr.Expr, allocator: std.mem.Allocator) ParseError!*Stmt {
        if (self.match(1, [1]TokenType{TokenType.BREAK}) and self.match(1, [1]TokenType{TokenType.SEMICOLON})) return s.Break.new(self.previous(), break_condition, allocator);
        if (self.match(1, [1]TokenType{TokenType.FOR})) return self.forStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.IF})) return self.breakIfStatement(break_condition, allocator);
        if (self.match(1, [1]TokenType{TokenType.PRINT})) return self.printStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.RETURN})) return self.returnStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.WHILE})) return self.whileStatement(allocator);
        if (self.match(1, [1]TokenType{TokenType.LEFT_BRACE})) return s.Block.new(try self.breakBlock(break_condition, allocator), allocator);
        return self.expressionStatement(allocator);
    }

    fn breakBlock(self: *Parser, condition: *expr.Expr, allocator: std.mem.Allocator) ParseError!ArrayList(*Stmt) {
        var break_count: u8 = 0;
        var statements: ArrayList(*s.Stmt) = .empty;
        // TODO: still issue with managing break count.
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) {
            if (break_count > 1) return err.errorMessage(ParseError, self.peek().line, "break appears multiple times in block", ParseError.SyntaxError, allocator);
            const stmt = try self.breakDeclaration(condition, allocator);
            if (stmt.* == .@"break") break_count += 1;

            try statements.append(allocator, stmt);
        }

        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect '}' at end of block");
        return statements;
    }

    fn breakIfStatement(self: *Parser, break_condition: *expr.Expr, allocator: std.mem.Allocator) ParseError!*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'");
        const condition = try self.expression(allocator);
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition");
        const then_branch = try self.breakStatement(break_condition, allocator);
        var else_branch: ?*Stmt = null;
        if (self.match(1, [1]TokenType{TokenType.ELSE})) {
            else_branch = try self.breakStatement(break_condition, allocator);
        }
        return try s.If.new(condition, then_branch, else_branch, allocator);
    }


    fn breakDeclaration(self: *Parser, break_condition: *expr.Expr, allocator: std.mem.Allocator) ParseError!*Stmt {
        try {
            if (self.match(1, [1]TokenType{TokenType.FUN})) return try self.function("function", allocator);
            if (self.match(1, [1]TokenType{TokenType.VAR})) return try self.varDeclaration(allocator);
            return try self.breakStatement(break_condition, allocator);
        } catch {
            self.synchronize();
            return Error.ParseError;
        };
    }

    fn block(self: *Parser, allocator: std.mem.Allocator) ParseError!ArrayList(*Stmt) {
        var statements: ArrayList(*s.Stmt) = .empty;
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) statements.append(allocator, try self.declaration(allocator)) catch return err.outOfMemory();
        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect '}' at end of block");
        return statements;
    }

    fn printStatement(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        const e = try self.expression(allocator); 
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value");
        return try s.Print.new(e, allocator);
    }
    
    fn expressionStatement(self: *Parser, allocator: std.mem.Allocator) ParseError!*Stmt {
        const e = try self.expression(allocator); 
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after expression");
        return try s.Expression.new(e, allocator);
    }
    
    fn expression(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        return try self.assignment(allocator);
    }

    fn assignment(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        // First naively evaluate the token before the '='
        const greedy_expr = try self.logic_or(allocator);
        var equals: Token = undefined;
        var value: *expr.Expr = undefined;

        if (self.match(1, [1]TokenType{TokenType.EQUAL})) {
            equals = self.previous();
            value = try self.assignment(allocator);

            switch (greedy_expr.*) {
                .@"var" => |@"var"| {
                    const name = @"var".name;
                    return try expr.Assign.new(name, value, allocator);
                },
                .get => |get| {
                    return try expr.Set.new(get.name, get.object, value, allocator);
                },
                else => {
                    // TODO: MESSAGE: expected either a variable or declaration on LHS of =
                    return VariableError.AssignmentError;
                }
            }
        }
        return greedy_expr;
    }

    fn logic_or(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        var left_expr = try self.logic_and(allocator);
        while (self.match(1, [1]TokenType{TokenType.OR})) {
            const operator = self.previous();
            const right = try self.logic_and(allocator);
            left_expr = try expr.Logical.new(left_expr, operator, right, allocator);
        }
        return left_expr;
    }

    fn logic_and(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr { 
        var left_expr = try self.equality(allocator);
        while (self.match(1, [1]TokenType{TokenType.AND})) {
            const operator = self.previous();
            const right = try self.equality(allocator);
            left_expr = try expr.Logical.new(left_expr, operator, right, allocator);
        }
        return left_expr;
    }

    fn equality(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        var e = try self.comparison(allocator);
        while (self.match(equality_match.len, equality_match)) {
            const operator = self.previous();
            const right = try self.comparison(allocator);
            e = try expr.Binary.new(e, operator, right, allocator);
        }
        return e;
    }

    fn comparison(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        var e = try self.term(allocator);
        while (self.match(comparison_match.len, comparison_match)) {
            const operator = self.previous();
            const right = try self.term(allocator);
            e = try expr.Binary.new(e, operator, right, allocator);
        }
        return e;
    }

    fn term(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        var e = try self.factor(allocator);
        while (self.match(term_match.len, term_match)) {
            const operator = self.previous();
            const right = try self.factor(allocator);
            e = try expr.Binary.new(e, operator, right, allocator);
        }
        return e;
    }

    fn factor(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        var e = try self.unary(allocator);
        while (self.match(factor_match.len, factor_match)) {
            const operator = self.previous();
            const right = try self.unary(allocator);
            e = try expr.Binary.new(e, operator, right, allocator);
        }
        return e;
    }

    fn unary(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        if (self.match(unary_match.len, unary_match)) {
            const operator = self.previous();
            const right = try self.unary(allocator);
            return try expr.Unary.new(operator, right, allocator);
        }
        return try self.call(allocator);
    }

    fn call(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        var builder_expr = try self.primary(allocator);
        while (true) {
            if (self.match(1, [1]TokenType{TokenType.LEFT_PAREN})) {
                builder_expr = try self.finishCall(builder_expr, allocator);
            } else if (self.match(1, [1]TokenType{TokenType.DOT})) {
                const name = try self.consume(TokenType.IDENTIFIER, "Expect property name after '.'.");
                builder_expr = try expr.Get.new(name, builder_expr, allocator);
            } else {
                break;
            }
        }
        return builder_expr;
    } 

    fn finishCall(self: *Parser, callee: *expr.Expr, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        var arguments: ArrayList(*expr.Expr) = .empty;
        if (!self.check(TokenType.RIGHT_PAREN)) {
            if (arguments.items.len >= 255) {
                return err.errorMessage(ParseError, self.peek().line, "Can't have more than 255 arguments", FunctionError.TooManyArguments, allocator);
            }

            arguments.append(allocator, try self.expression(allocator)) catch return err.outOfMemory();
            while (self.match(1, [1]TokenType{TokenType.COMMA})) {
                arguments.append(allocator, try self.expression(allocator)) catch return err.outOfMemory();
            }
        }
        const right_paren = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments");
        return try expr.Call.new(callee, right_paren, arguments, allocator);
    }

    fn anonymousFunction(self: *Parser, allocator: std.mem.Allocator) ParseError!*s.Stmt {
        const paren = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after anonymous function expression");
        var parameters: ArrayList(Token) = .empty;
        while (!self.check(TokenType.RIGHT_PAREN)) {
            if (parameters.items.len >= 255) return err.errorMessage(ParseError, self.peek().line, "Can't have more than 255 parameters", ParseError.TooManyArguments, allocator);

            parameters.append(allocator, try self.consume(TokenType.IDENTIFIER, "Expect parameter name")) catch return err.outOfMemory();
            _ = self.match(1, [1]TokenType{TokenType.COMMA});
        }
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters");
        _ = try self.consume(TokenType.LEFT_BRACE, "Expect '{{' before anonymous function body");
        const body = try self.block(allocator);
        return try s.Function.new(Token.new(TokenType.NIL, "", null, paren.line), parameters, body, allocator);
    }

    fn anonymous(self: *Parser, keyword: Token, allocator: std.mem.Allocator) ParseError!*expr.Expr{
        const func: s.Function = (try self.anonymousFunction(allocator)).function;
        return try expr.Anonymous.new(keyword, func, allocator);
    }

    fn primary(self: *Parser, allocator: std.mem.Allocator) ParseError!*expr.Expr {
        if (self.match(1, [1]TokenType{TokenType.FALSE})) return try expr.Literal.new(Object{.Bool=false}, allocator);
        if (self.match(1, [1]TokenType{TokenType.TRUE})) return try expr.Literal.new(Object{.Bool=true}, allocator);
        if (self.match(1, [1]TokenType{TokenType.NIL})) return try expr.Literal.new(Object{.Nil=null}, allocator);
        // NOTE: want Literal and Identifiers to persist outside scope, so we need to heap allocate and copy to allocated memory
        if (self.match(2, [2]TokenType{TokenType.NUMBER, TokenType.STRING})) return try expr.Literal.new(self.previous().literal, allocator);
        if (self.match(1, [1]TokenType{TokenType.SUPER})) {
            const keyword = self.previous();
            _ = try self.consume(TokenType.DOT, "Expect '.' after 'super'");
            const method = try self.consume(TokenType.IDENTIFIER, "Expect superclass method name");
            return try expr.Super.new(keyword, method, allocator);
        }
        if (self.match(1, [1]TokenType{TokenType.THIS})) return try expr.This.new(self.previous(), allocator);
        if (self.match(1, [1]TokenType{TokenType.IDENTIFIER})) return try expr.Var.new(self.previous(), allocator);
        // TODO: does it make sense to make anonymous a primary
        if (self.match(1, [1]TokenType{TokenType.FUN})) return try self.anonymous(self.previous(), allocator);

        if (self.match(1, [1]TokenType{TokenType.LEFT_PAREN})) {
            const e = try self.expression(allocator);
            _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
            return try expr.Grouping.new(e, allocator);
        }
        return err.errorMessage(ParseError, self.peek().line, "Expecting an expression before `;`", ParseError.SyntaxError, allocator);
    }

    fn synchronize(self: *Parser) void {
        self.advance();
        while (!self.reachedEnd()) {
            if (self.previous().ttype == TokenType.SEMICOLON) return;

            switch (self.peek().ttype) {
                TokenType.CLASS, TokenType.FOR, TokenType.FUN, TokenType.IF, TokenType.PRINT, TokenType.RETURN, TokenType.VAR, TokenType.WHILE => {return;},
                else => {}
            }
            self.advance();
        }
    }

    fn consume(self: *Parser, ttype: TokenType, error_msg: []const u8) ParseError!Token {
        if (self.check(ttype)) return self.advance();
        const error_token = self.peek();
        if (error_token.ttype == TokenType.EOF) {
            std.debug.print("[line: {d}, near end] {s}\n",.{error_token.line, error_msg});
            return ParseError.EndOfFile;
        } 
        std.debug.print("[line: {d}, near '{s}'] {s}\n",.{error_token.line, error_token.lexeme, error_msg});
        
        // TODO: maps all ttypes to appropriate errors
        // const mapped_error = switch (ttype) {
        //     else => {}
        // };
        return ParseError.SyntaxError;
    }
    
    // HELPER FUNCTIONS
    fn match(self: *Parser, comptime len: usize, types: [len]TokenType) bool { 
        for (types) |ttype| {
            if (self.check(ttype)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }
 
    fn check(self: *Parser, ttype: TokenType) bool { 
        if (self.reachedEnd()) return false;
        return self.peek().ttype == ttype;
    }

    fn advance(self: *Parser) Token { 
        if (!self.reachedEnd()) self.current += 1;
        return self.previous();
    }

    fn reachedEnd(self: *Parser) bool { 
        return self.peek().ttype == TokenType.EOF;
    }

    fn peek(self: *Parser) Token { 
        return self.tokens.items[self.current];
    }

    fn previous(self: *Parser) Token { 
        return self.tokens.items[self.current-1];
    }

};

test "parser: helper method (`advance`, `peek`, `previous`) functionality" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tokens: ArrayList(Token) = .empty;
    defer tokens.deinit(allocator);
    try tokens.append(allocator, Token.new(TokenType.NUMBER, "1", "1", 1));
    try tokens.append(allocator, Token.new(TokenType.STAR, "*", null, 1));
    try tokens.append(allocator, Token.new(TokenType.NUMBER, "2", "2", 1));
    try tokens.append(allocator, Token.new(TokenType.EOF, "", null, 1));

    var parser = Parser.new(tokens, allocator);
    try std.testing.expectEqual(false, parser.check(TokenType.EOF));
    try std.testing.expectEqual(true, parser.check(TokenType.NUMBER));
    try std.testing.expectEqual(false, parser.reachedEnd());
    
    // Take a step
    try std.testing.expectEqual(Token.new(TokenType.NUMBER, "1", "1", 1), parser.advance());
    try std.testing.expectEqual(Token.new(TokenType.NUMBER, "1", "1", 1), parser.previous());
    try std.testing.expectEqual(Token.new(TokenType.STAR, "*", null, 1), parser.peek());
    try std.testing.expectEqual(false, parser.reachedEnd());

    // Take another step
    try std.testing.expectEqual(Token.new(TokenType.STAR, "*", null, 1), parser.advance());
    try std.testing.expectEqual(Token.new(TokenType.STAR, "*", null, 1), parser.previous());
    try std.testing.expectEqual(Token.new(TokenType.NUMBER, "2", "2", 1), parser.peek());
    try std.testing.expectEqual(false, parser.reachedEnd());

    // Step til end 
    _ = parser.advance();
    try std.testing.expectEqual(true, parser.reachedEnd());
}

test "yes" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    _ = allocator;
}

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
    allocator: std.mem.Allocator,

    pub fn init(tokens: ArrayList(Token), allocator: std.mem.Allocator) Parser {
        return Parser{ .tokens = tokens, .allocator = allocator};
    }

    // TODO: double-check logic for freeing! Need a better understanding of memory
    // pub fn deinit(self: *Parser) void {
    //     self.allocator.free(self.tokens);
    // }
 
    pub fn parse(self: *Parser) ParseError!ArrayList(*Stmt) {
        var statements = ArrayList(*Stmt).init(self.allocator);
        // TODO: do we need to dealloc the ArrayList(Stmt)
        while (!self.reachedEnd()) {
            statements.append(try self.declaration()) catch return err.outOfMemory();
        }
        return statements;
    }

    fn declaration(self: *Parser) ParseError!*Stmt {
        try {
            if (self.match(1, [1]TokenType{TokenType.CLASS})) return try self.classDeclaration();
            if (self.match(1, [1]TokenType{TokenType.FUN})) return try self.function("function");
            if (self.match(1, [1]TokenType{TokenType.VAR})) return try self.varDeclaration();
            return try self.statement();
        } catch {
            self.synchronize();
            return ParseError.SyntaxError;
        };
    }

    fn classDeclaration(self: *Parser) ParseError!*Stmt {
        const name = try self.consume(TokenType.IDENTIFIER, "Expect class name.");
        var superclass: ?*expr.Expr = null;
        if (self.match(1, [1]TokenType{TokenType.LESS})) {
            _ = try self.consume(TokenType.IDENTIFIER, "Expect superclass name.");
            superclass = expr.Var.new(self.previous(), self.allocator) catch return ParseError.SyntaxError;
        }         
        _ = try self.consume(TokenType.LEFT_BRACE, "Expect '{' before class body.");

        var methods = ArrayList(*s.Stmt).init(self.allocator);
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) {
            try methods.append(try self.function("method"));
        }

        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.");
 
        return s.Class.new(name, superclass, methods, self.allocator);
    }

    fn function(self: *Parser, kind: []const u8) ParseError!*Stmt {
        var err_msg = std.fmt.allocPrint(self.allocator, "Expect {s} name", .{kind}) catch return err.outOfMemory();
        const name = try self.consume(TokenType.IDENTIFIER, err_msg);
        err_msg = std.fmt.allocPrint(self.allocator, "Expect '(' after {s} name", .{kind}) catch return err.outOfMemory();
        _ = try self.consume(TokenType.LEFT_PAREN, "");

        var parameters = ArrayList(Token).init(self.allocator);
        while (!self.check(TokenType.RIGHT_PAREN)) {
            if (parameters.items.len >= 255) return err.errorMessage(ParseError, self.peek().line, "Can't have more than 255 parameters", ParseError.TooManyArguments, self.allocator);

            parameters.append(try self.consume(TokenType.IDENTIFIER, "Expect parameter name")) catch return err.outOfMemory();
            _ = self.match(1, [1]TokenType{TokenType.COMMA});
        }
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters");
        err_msg = std.fmt.allocPrint(self.allocator, "Expect '{{' before {s} body", .{kind}) catch return err.outOfMemory();
        _ = try self.consume(TokenType.LEFT_BRACE, err_msg);
        const body = try self.block();
        return s.Function.new(name, parameters, body, self.allocator);
    }


    fn varDeclaration(self: *Parser) ParseError!*Stmt {
        const name = try self.consume(TokenType.IDENTIFIER, "Expect variable name");
        var initializer: ?*expr.Expr = null; 

        if (self.match(1, [1]TokenType{TokenType.EQUAL})) initializer = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration");
        return s.Var.new(name, initializer, self.allocator);
    }

    fn statement(self: *Parser) ParseError!*Stmt {
        if (self.match(1, [1]TokenType{TokenType.BREAK})) return err.errorMessage(ParseError, self.peek().line, "break not nested within control flow", ParseError.SyntaxError, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.FOR})) return self.forStatement();
        if (self.match(1, [1]TokenType{TokenType.IF})) return self.ifStatement();
        if (self.match(1, [1]TokenType{TokenType.PRINT})) return self.printStatement();
        if (self.match(1, [1]TokenType{TokenType.RETURN})) return self.returnStatement();
        if (self.match(1, [1]TokenType{TokenType.WHILE})) return self.whileStatement();
        if (self.match(1, [1]TokenType{TokenType.LEFT_BRACE})) return s.Block.new(try self.block(), self.allocator);
        return self.expressionStatement();
    }

    fn forStatement(self: *Parser) ParseError!*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'");
        var initializer: ?*Stmt = undefined;
        if (self.match(1, [1]TokenType{TokenType.SEMICOLON})) {
            initializer = null;
        } else if (self.match(1, [1]TokenType{TokenType.VAR})) {
            initializer = try self.varDeclaration();
        } else {
            initializer = try self.expressionStatement();
        }

        var condition: ?*expr.Expr = null;
        if (!self.check(TokenType.SEMICOLON)) {
            condition = try self.expression();
        } else {
            condition = try expr.Literal.new(Object{.Bool = true}, self.allocator);
        }
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after loop condition");

        var increment: ?*expr.Expr = null;
        if (!self.check(TokenType.RIGHT_PAREN)) {
            increment = try self.expression();
        }         
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition");

        var body = try self.breakStatement(condition.?);

        if (increment != null) {
            var statements = ArrayList(*Stmt).init(self.allocator);
            statements.append(body) catch return err.outOfMemory();
            statements.append(try s.Expression.new(increment.?, self.allocator)) catch return err.outOfMemory();
            body = try s.Block.new(statements, self.allocator);
        }

        body = try s.While.new(condition.?, body, self.allocator);

        if (initializer != null) {
            var statements = ArrayList(*Stmt).init(self.allocator);
            statements.append(initializer.?) catch return err.outOfMemory();
            statements.append(body) catch return err.outOfMemory();
            body = try s.Block.new(statements, self.allocator);
        }
        return body;
    }

    fn returnStatement(self: *Parser) ParseError!*Stmt {
        const keyword  = self.previous();
        var value: ?*expr.Expr = null;
        if (!self.check(TokenType.SEMICOLON)) {
            value = try self.expression();
        }
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after 'return'");
        return s.Return.new(keyword, value, self.allocator);
    }

    fn whileStatement(self: *Parser) ParseError!*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'");
        const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition");
        const body = try self.breakStatement(condition);
        return try s.While.new(condition, body, self.allocator);
    }

    fn ifStatement(self: *Parser) ParseError!*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'");
        const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition");
        const then_branch = try self.statement();
        var else_branch: ?*Stmt = null;
        if (self.match(1, [1]TokenType{TokenType.ELSE})) {
            else_branch = try self.statement();
        }
        return try s.If.new(condition, then_branch, else_branch, self.allocator);
    }


    fn breakStatement(self: *Parser, break_condition: *expr.Expr) ParseError!*Stmt {
        if (self.match(1, [1]TokenType{TokenType.BREAK}) and self.match(1, [1]TokenType{TokenType.SEMICOLON})) return s.Break.new(self.previous(), break_condition, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.FOR})) return self.forStatement();
        if (self.match(1, [1]TokenType{TokenType.IF})) return self.breakIfStatement(break_condition);
        if (self.match(1, [1]TokenType{TokenType.PRINT})) return self.printStatement();
        if (self.match(1, [1]TokenType{TokenType.RETURN})) return self.returnStatement();
        if (self.match(1, [1]TokenType{TokenType.WHILE})) return self.whileStatement();
        if (self.match(1, [1]TokenType{TokenType.LEFT_BRACE})) return s.Block.new(try self.breakBlock(break_condition), self.allocator);
        return self.expressionStatement();
    }

    fn breakBlock(self: *Parser, condition: *expr.Expr) ParseError!ArrayList(*Stmt) {
        var break_count: u8 = 0;
        var statements = ArrayList(*s.Stmt).init(self.allocator);
        // TODO: still issue with managing break count.
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) {
            if (break_count > 1) return err.errorMessage(ParseError, self.peek().line, "break appears multiple times in block", ParseError.SyntaxError, self.allocator);
            const stmt = try self.breakDeclaration(condition);
            if (stmt.* == .@"break") break_count += 1;

            try statements.append(stmt);
        }

        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect '}' at end of block");
        return statements;
    }

    fn breakIfStatement(self: *Parser, break_condition: *expr.Expr) ParseError!*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'");
        const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition");
        const then_branch = try self.breakStatement(break_condition);
        var else_branch: ?*Stmt = null;
        if (self.match(1, [1]TokenType{TokenType.ELSE})) {
            else_branch = try self.breakStatement(break_condition);
        }
        return try s.If.new(condition, then_branch, else_branch, self.allocator);
    }


    fn breakDeclaration(self: *Parser, break_condition: *expr.Expr) ParseError!*Stmt {
        try {
            if (self.match(1, [1]TokenType{TokenType.FUN})) return try self.function("function");
            if (self.match(1, [1]TokenType{TokenType.VAR})) return try self.varDeclaration();
            return try self.breakStatement(break_condition);
        } catch {
            self.synchronize();
            return Error.ParseError;
        };
    }

    fn block(self: *Parser) ParseError!ArrayList(*Stmt) {
        var statements = ArrayList(*s.Stmt).init(self.allocator);
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) statements.append(try self.declaration()) catch return err.outOfMemory();
        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect '}' at end of block");
        return statements;
    }

    fn printStatement(self: *Parser) ParseError!*Stmt {
        const e = try self.expression(); 
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value");
        return try s.Print.new(e, self.allocator);
    }
    
    fn expressionStatement(self: *Parser) ParseError!*Stmt {
        const e = try self.expression(); 
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after expression");
        return try s.Expression.new(e, self.allocator);
    }
    
    fn expression(self: *Parser) ParseError!*expr.Expr {
        return try self.assignment();
    }

    fn assignment(self: *Parser) ParseError!*expr.Expr {
        // First naively evaluate the token before the '='
        const greedy_expr = try self.logic_or();
        var equals: Token = undefined;
        var value: *expr.Expr = undefined;

        if (self.match(1, [1]TokenType{TokenType.EQUAL})) {
            equals = self.previous();
            value = try self.assignment();

            switch (greedy_expr.*) {
                .@"var" => |@"var"| {
                    const name = @"var".name;
                    return try expr.Assign.new(name, value, self.allocator);
                },
                .get => |get| {
                    return try expr.Set.new(get.name, get.object, value, self.allocator);
                },
                else => {
                    // TODO: MESSAGE: expected either a variable or declaration on LHS of =
                    return VariableError.AssignmentError;
                }
            }
        }
        return greedy_expr;
    }

    fn logic_or(self: *Parser) ParseError!*expr.Expr {
        var left_expr = try self.logic_and();
        while (self.match(1, [1]TokenType{TokenType.OR})) {
            const operator = self.previous();
            const right = try self.logic_and();
            left_expr = try expr.Logical.new(left_expr, operator, right, self.allocator);
        }
        return left_expr;
    }

    fn logic_and(self: *Parser) ParseError!*expr.Expr { 
        var left_expr = try self.equality();
        while (self.match(1, [1]TokenType{TokenType.AND})) {
            const operator = self.previous();
            const right = try self.equality();
            left_expr = try expr.Logical.new(left_expr, operator, right, self.allocator);
        }
        return left_expr;
    }

    fn equality(self: *Parser) ParseError!*expr.Expr {
        var e = try self.comparison();
        while (self.match(equality_match.len, equality_match)) {
            const operator = self.previous();
            const right = try self.comparison();
            e = try expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn comparison(self: *Parser) ParseError!*expr.Expr {
        var e = try self.term();
        while (self.match(comparison_match.len, comparison_match)) {
            const operator = self.previous();
            const right = try self.term();
            e = try expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn term(self: *Parser) ParseError!*expr.Expr {
        var e = try self.factor();
        while (self.match(term_match.len, term_match)) {
            const operator = self.previous();
            const right = try self.factor();
            e = try expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn factor(self: *Parser) ParseError!*expr.Expr {
        var e = try self.unary();
        while (self.match(factor_match.len, factor_match)) {
            const operator = self.previous();
            const right = try self.unary();
            e = try expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn unary(self: *Parser) ParseError!*expr.Expr {
        if (self.match(unary_match.len, unary_match)) {
            const operator = self.previous();
            const right = try self.unary();
            return try expr.Unary.new(operator, right, self.allocator);
        }
        return try self.call();
    }

    fn call(self: *Parser) ParseError!*expr.Expr {
        var builder_expr = try self.primary();
        while (true) {
            if (self.match(1, [1]TokenType{TokenType.LEFT_PAREN})) {
                builder_expr = try self.finishCall(builder_expr);
            } else if (self.match(1, [1]TokenType{TokenType.DOT})) {
                const name = try self.consume(TokenType.IDENTIFIER, "Expect property name after '.'.");
                builder_expr = try expr.Get.new(name, builder_expr, self.allocator);
            } else {
                break;
            }
        }
        return builder_expr;
    } 

    fn finishCall(self: *Parser, callee: *expr.Expr) ParseError!*expr.Expr {
        var arguments = ArrayList(*expr.Expr).init(self.allocator);
        if (!self.check(TokenType.RIGHT_PAREN)) {
            if (arguments.items.len >= 255) {
                return err.errorMessage(ParseError, self.peek().line, "Can't have more than 255 arguments", FunctionError.TooManyArguments, self.allocator);
            }

            arguments.append(try self.expression()) catch return err.outOfMemory();
            while (self.match(1, [1]TokenType{TokenType.COMMA})) {
                arguments.append(try self.expression()) catch return err.outOfMemory();
            }
        }
        const right_paren = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments");
        return try expr.Call.new(callee, right_paren, arguments, self.allocator);
    }

    fn anonymousFunction(self: *Parser) ParseError!*s.Stmt {
        const paren = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after anonymous function expression");
        var parameters = ArrayList(Token).init(self.allocator);
        while (!self.check(TokenType.RIGHT_PAREN)) {
            if (parameters.items.len >= 255) return err.errorMessage(ParseError, self.peek().line, "Can't have more than 255 parameters", ParseError.TooManyArguments, self.allocator);

            parameters.append(try self.consume(TokenType.IDENTIFIER, "Expect parameter name")) catch return err.outOfMemory();
            _ = self.match(1, [1]TokenType{TokenType.COMMA});
        }
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters");
        _ = try self.consume(TokenType.LEFT_BRACE, "Expect '{{' before anonymous function body");
        const body = try self.block();
        return try s.Function.new(Token.new(TokenType.NIL, "", null, paren.line), parameters, body, self.allocator);
    }

    fn anonymous(self: *Parser, keyword: Token) ParseError!*expr.Expr{
        const func: s.Function = (try self.anonymousFunction()).function;
        return try expr.Anonymous.new(keyword, func, self.allocator);
    }

    fn primary(self: *Parser) ParseError!*expr.Expr {
        if (self.match(1, [1]TokenType{TokenType.FALSE})) return try expr.Literal.new(Object{.Bool=false}, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.TRUE})) return try expr.Literal.new(Object{.Bool=true}, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.NIL})) return try expr.Literal.new(Object{.Nil=null}, self.allocator);
        // NOTE: want Literal and Identifiers to persist outside scope, so we need to heap allocate and copy to allocated memory
        if (self.match(2, [2]TokenType{TokenType.NUMBER, TokenType.STRING})) return try expr.Literal.new(self.previous().literal, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.SUPER})) {
            const keyword = self.previous();
            _ = try self.consume(TokenType.DOT, "Expect '.' after 'super'");
            const method = try self.consume(TokenType.IDENTIFIER, "Expect superclass method name");
            return try expr.Super.new(keyword, method, self.allocator);
        }
        if (self.match(1, [1]TokenType{TokenType.THIS})) return try expr.This.new(self.previous(), self.allocator);
        if (self.match(1, [1]TokenType{TokenType.IDENTIFIER})) return try expr.Var.new(self.previous(), self.allocator);
        // TODO: does it make sense to make anonymous a primary
        if (self.match(1, [1]TokenType{TokenType.FUN})) return try self.anonymous(self.previous());

        if (self.match(1, [1]TokenType{TokenType.LEFT_PAREN})) {
            const e = try self.expression();
            _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
            return try expr.Grouping.new(e, self.allocator);
        }
        return err.errorMessage(ParseError, self.peek().line, "Expecting an expression before `;`", ParseError.SyntaxError, self.allocator);
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
    var tokens = ArrayList(Token).init(allocator);
    defer tokens.deinit();
    try tokens.append(Token.new(TokenType.NUMBER, "1", "1", 1));
    try tokens.append(Token.new(TokenType.STAR, "*", null, 1));
    try tokens.append(Token.new(TokenType.NUMBER, "2", "2", 1));
    try tokens.append(Token.new(TokenType.EOF, "", null, 1));

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

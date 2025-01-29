const std = @import("std");

const ArrayList = std.ArrayList;

const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const Object = @import("token/object.zig").Object;

const expr = @import("expr.zig");

const Stmt = @import("stmt.zig").Stmt;
const s = @import("stmt.zig");

const Error = @import("error.zig").Error;
const err = @import("error.zig");

// TODO: Add more granular errors for ParserErrors
const equality_match = [2]TokenType{TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL};
const unary_match = [2]TokenType{TokenType.BANG, TokenType.MINUS};
const factor_match = [2]TokenType{TokenType.SLASH, TokenType.STAR};
const comparison_match = [4]TokenType{TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL};
const term_match = [2]TokenType{TokenType.MINUS, TokenType.PLUS};

// TODO: need to reason better about when to heap allocate
// TODO: implement better error messaging in the parser
pub const Parser = struct {
    tokens: ArrayList(Token),
    current: u64 = 0,
    allocator: std.mem.Allocator,

    pub fn new(tokens: ArrayList(Token), allocator: std.mem.Allocator) Parser {
        return Parser{ .tokens = tokens, .allocator = allocator};
    }

    pub fn parse(self: *Parser) Error!ArrayList(Stmt) {
        var statements = ArrayList(Stmt).init(self.allocator);
        // TODO: do we need to dealloc the ArrayList(Stmt)
        while (!self.reachedEnd()) {
            try statements.append(try self.declaration());
        }
        return statements;
        // return self.expression() catch return Error.ParseError; 
        //std.debug.panic("Panicked due to Parse Error");
    }

    // Parsing Statements
    //
    // STATEMENT GRAMMAR RULES
    // program        -> declaration * EOF ; 
    //
    // declaration    -> funDecl 
    //                   | varDecl 
    //                   | statement ;
    //
    // funDecl        -> "fun" function ;
    //
    // function       -> IDENTIFIER "(" parameters? ")" block ;
    //
    // parameters     -> IDENTIFIER ( "," IDENTIFIER )* ;
    //
    // varDecl        -> "var" IDENTIFIER ( "=" expression )? ";" ;
    //
    // statement      -> exprStmt
    //                   | forStmt
    //                   | ifStmt
    //                   | printStmt
    //                   | whileStmt
    //                   | block ;
    //
    // returnStmt     -> "return" expression? ";" ;
    //                      
    // forStmt        -> "for" "(" ( varDecl | exprStmt | ; )
    //                   expression? ";"
    //                   expression? ")" break_statement ;
    //
    // whileStmt      -> "while" "(" expression ")" break_statement ;
    //
    // breakStatement -> break 
    //                   | exprStmt
    //                   | forStmt
    //                   | breakIfStmt
    //                   | printStmt
    //                   | whileStm 
    //                   | breakBlock ;
    //
    // breakIfStmt    -> if "(" expression ")" breakStatement
    //                   ( "else" breakStatement )? ;  
    //
    // breakBlock     -> "{" breakDecl* "}" ;
    //
    // breakDecl      -> funDecl
    //                   | varDecl 
    //                   | breakStatement ;
    //
    // ifStmt         -> if "(" expression ")" statement
    //                   ( "else" statement )? ;  
    //
    // block          -> "{" declaration* "}" ;
    
    fn declaration(self: *Parser) Error!Stmt {
        try {
            if (self.match(1, [1]TokenType{TokenType.FUN})) return try self.function("function");
            if (self.match(1, [1]TokenType{TokenType.VAR})) return try self.varDeclaration();
            return try self.statement();
        } catch {
            self.synchronize();
            return Error.ParseError;
        };
    }

    fn function(self: *Parser, kind: []const u8) Error!Stmt {
        const name = try self.consume(TokenType.IDENTIFIER, try std.fmt.allocPrint(self.allocator, "Expect {s} name", .{kind}));
        _ = try self.consume(TokenType.LEFT_PAREN, try std.fmt.allocPrint(self.allocator, "Expect '(' after {s} name", .{kind}));
        var parameters = ArrayList(Token).init(self.allocator);
        while (!self.check(TokenType.RIGHT_PAREN)) {
            if (parameters.items.len >= 255) return err.error_msg(self.peek().line, "Can't have more than 255 parameters", Error.TooManyArguments, self.allocator);
            try parameters.append(try self.consume(TokenType.IDENTIFIER, "Expect parameter name"));
            _ = self.match(1, [1]TokenType{TokenType.COMMA});
        }
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after paremeters");
        _ = try self.consume(TokenType.LEFT_BRACE, try std.fmt.allocPrint(self.allocator, "Expect '{c}' before {s} body", .{'{', kind}));
        const body = try self.block();
        return s.Function.new(name, parameters, body);
    }

    fn varDeclaration(self: *Parser) Error!Stmt {
        const name = try self.consume(TokenType.IDENTIFIER, "Expect variable name");
        var initializer: ?*expr.Expr = null; 

        if (self.match(1, [1]TokenType{TokenType.EQUAL})) initializer = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration");
        return s.Var.new(name, initializer);
    }

    fn statement(self: *Parser) Error!Stmt {
        if (self.match(1, [1]TokenType{TokenType.BREAK})) return err.error_msg(self.peek().line, "break not nested within control flow", Error.SyntaxError, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.FOR})) return self.forStatement();
        if (self.match(1, [1]TokenType{TokenType.IF})) return self.ifStatement();
        if (self.match(1, [1]TokenType{TokenType.PRINT})) return self.printStatement();
        if (self.match(1, [1]TokenType{TokenType.WHILE})) return self.whileStatement();
        if (self.match(1, [1]TokenType{TokenType.LEFT_BRACE})) return s.Block.new(try self.block());
        return self.expressionStatement();
    }

    fn forStatement(self: *Parser) Error!Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'");
        var initializer: ?Stmt = undefined;
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
        } 
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after loop condition");

        var increment: ?*expr.Expr = null;
        if (!self.check(TokenType.RIGHT_PAREN)) {
            increment = try self.expression();
        }         
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition");

        condition = expr.Literal.new(Object{.Bool = true}, self.allocator);
        var body = try self.breakStatement(condition.?);

        if (increment != null) {
            var statements = ArrayList(Stmt).init(self.allocator);

            statements.append(body) catch return Error.AllocError;
            statements.append(s.Expression.new(increment.?)) catch return Error.AllocError;
            body = s.Block.new(statements);
        }

        body = try s.While.new(condition.?, body, self.allocator);

        if (initializer != null) {
            var statements = ArrayList(Stmt).init(self.allocator);
            statements.append(initializer.?) catch return Error.AllocError;
            statements.append(body) catch return Error.AllocError;
            body = s.Block.new(statements);
        }
        return body;
    }

    fn whileStatement(self: *Parser) Error!Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'");
        const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition");
        const body = try self.breakStatement(condition);
        return s.While.new(condition, body, self.allocator);
    }

    fn ifStatement(self: *Parser) Error!Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'");
        const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition");
        const then_branch = try self.statement();
        var else_branch: ?Stmt = null;
        if (self.match(1, [1]TokenType{TokenType.ELSE})) {
            else_branch = try self.statement();
        }
        return try s.If.new(condition, then_branch, else_branch, self.allocator);
    }


    fn breakStatement(self: *Parser, break_condition: *expr.Expr) Error!Stmt {
        // TODO: issue with identifying multiple break's per block
        if (self.match(1, [1]TokenType{TokenType.BREAK}) and self.match(1, [1]TokenType{TokenType.SEMICOLON})) return s.Break.new(break_condition);
        if (self.match(1, [1]TokenType{TokenType.FOR})) return self.forStatement();
        if (self.match(1, [1]TokenType{TokenType.IF})) return self.breakIfStatement(break_condition);
        if (self.match(1, [1]TokenType{TokenType.PRINT})) return self.printStatement();
        if (self.match(1, [1]TokenType{TokenType.WHILE})) return self.whileStatement();
        if (self.match(1, [1]TokenType{TokenType.LEFT_BRACE})) return s.Block.new(try self.breakBlock(break_condition));
        return self.expressionStatement();
    }

    fn breakBlock(self: *Parser, condition: *expr.Expr) Error!ArrayList(Stmt) {
        var break_count: u8 = 0;
        var statements = ArrayList(s.Stmt).init(self.allocator);
        // TODO: still issue with managing break count.
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) {
            if (break_count > 1) return err.error_msg(self.peek().line, "break appears multiple times in block", Error.SyntaxError, self.allocator);
            const stmt = try self.breakDeclaration(condition);
            switch (stmt) {
                .@"break" => break_count += 1,
                else => {}
            }
            try statements.append(stmt);
        }
        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect ';' after block");
        return statements;
    }

    fn breakIfStatement(self: *Parser, break_condition: *expr.Expr) Error!Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'");
        const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition");
        const then_branch = try self.breakStatement(break_condition);
        var else_branch: ?Stmt = null;
        if (self.match(1, [1]TokenType{TokenType.ELSE})) {
            else_branch = try self.breakStatement(break_condition);
        }
        return try s.If.new(condition, then_branch, else_branch, self.allocator);
    }


    fn breakDeclaration(self: *Parser, break_condition: *expr.Expr) Error!Stmt {
        try {
            if (self.match(1, [1]TokenType{TokenType.FUN})) return try self.function("function");
            if (self.match(1, [1]TokenType{TokenType.VAR})) return try self.varDeclaration();
            return try self.breakStatement(break_condition);
        } catch {
            self.synchronize();
            return Error.ParseError;
        };
    }

    fn block(self: *Parser) Error!ArrayList(Stmt) {
        var statements = ArrayList(s.Stmt).init(self.allocator);
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) try statements.append(try self.declaration());
        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect '}' at end of block");
        return statements;
    }
    fn printStatement(self: *Parser) Error!Stmt {
        const e = self.expression() catch return Error.ParseError; 
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after value");
        return s.Print.new(e);
    }
    
    fn expressionStatement(self: *Parser) Error!Stmt {
        const e = self.expression() catch return Error.ParseError; 
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after expression");
        return s.Expression.new(e);
    }
    
    // Parsing Expressions
    //
    // EXPRESSION GRAMMAR RULES
    // expression     -> assignment ;
    // assignment     -> IDENTIFIER "=" assignment 
    //                   | logic_or 
    // logic_or       -> logic_and ( "or" logic_and )* ;
    // logic_and      -> equality ( "and" equality )*;
    // equality       -> comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison     -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term           -> factor ( ( "-" | "+" ) factor )* ;
    // factor         -> unary ( ( "/" | "*" ) unary )* ;
    // unary          -> ( "!" | "-" ) unary
    //                   | call;
    // call           -> primary ( "(" arguments? ")" )* ;
    // arguments      -> expression ( "," expression )* ;
    // primary        -> NUMBER | STRING | "true" | "false" | "nil"
    //                   | "(" expression ")" ;

    fn expression(self: *Parser) Error!*expr.Expr {
        return try self.assignment();
    }

    fn assignment(self: *Parser) Error!*expr.Expr {
        // First naively evaluate the token before the '='
        const greedy_expr = try self.logic_or();
        var equals: Token = undefined;
        var value: *expr.Expr = undefined;

        // Check if we are indeed "assigning" 
        // Step-forward tokens until '=' 
        if (self.match(1, [1]TokenType{TokenType.EQUAL})) {
            equals = self.previous();
            value = try self.assignment();

            // Ensure that the naively evaluated token is a Var (variable)
            switch (greedy_expr.*) {
                .@"var"=> |@"var"| {
                    const name = @"var".name;
                    return expr.Assign.new(name, value, self.allocator);
                },
                else => {
                    return Error.AssignmentError;
                }
            }
        }
        return greedy_expr;
    }

    fn logic_or(self: *Parser) Error!*expr.Expr {
        var left_expr = try self.logic_and();

        while (self.match(1, [1]TokenType{TokenType.OR})) {
            const operator = self.previous();
            const right = try self.logic_and();
            left_expr = expr.Logical.new(left_expr, operator, right, self.allocator);
        }
        return left_expr;
    }

    fn logic_and(self: *Parser) Error!*expr.Expr { 
        var left_expr = try self.equality();

        while (self.match(1, [1]TokenType{TokenType.AND})) {
            const operator = self.previous();
            const right = try self.equality();
            left_expr = expr.Logical.new(left_expr, operator, right, self.allocator);
        }
        return left_expr;
    }

    fn equality(self: *Parser) Error!*expr.Expr {
        var e = try self.comparison();
        while (self.match(equality_match.len, equality_match)) {
            const operator = self.previous();
            const right = try self.comparison();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn comparison(self: *Parser) Error!*expr.Expr {
        var e = try self.term();
        while (self.match(comparison_match.len, comparison_match)) {
            const operator = self.previous();
            const right = try self.term();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn term(self: *Parser) Error!*expr.Expr {
        var e = try self.factor();
        while (self.match(term_match.len, term_match)) {
            const operator = self.previous();
            const right = try self.factor();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn factor(self: *Parser) Error!*expr.Expr {
        var e = try self.unary();
        while (self.match(factor_match.len, factor_match)) {
            const operator = self.previous();
            const right = try self.unary();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn unary(self: *Parser) Error!*expr.Expr {
        if (self.match(unary_match.len, unary_match)) {
            const operator = self.previous();
            const right = try self.unary();
            return expr.Unary.new(operator, right, self.allocator);
        }
        return try self.call();
    }

    fn call(self: *Parser) Error!*expr.Expr {
        var builder_expr = try self.primary();
        while (true) {
            if (self.match(1, [1]TokenType{TokenType.LEFT_PAREN})) {
                builder_expr = try self.finishCall(builder_expr);
            } else {
                break;
            }
        }
        return builder_expr;
    } 

    fn finishCall(self: *Parser, callee: *expr.Expr) Error!*expr.Expr {
        var arguments = ArrayList(*expr.Expr).init(self.allocator);
        if (!self.check(TokenType.RIGHT_PAREN)) {
            if (arguments.items.len >= 255) {
                return err.error_msg(self.peek().line, "Can't have more than 255 arguments", Error.TooManyArguments, self.allocator);
            }
            try arguments.append(try self.expression());
            while (self.match(1, [1]TokenType{TokenType.COMMA})) {
                try arguments.append(try self.expression());
            }
        }

        const right_paren = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments");
        // TODO: the fuck is callee in this case
        return expr.Call.new(callee, right_paren, arguments, self.allocator);

    }

    fn primary(self: *Parser) Error!*expr.Expr {
        if (self.match(1, [1]TokenType{TokenType.FALSE})) return expr.Literal.new(Object{.Bool=false}, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.TRUE})) return expr.Literal.new(Object{.Bool=true}, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.NIL})) return expr.Literal.new(Object{.Nil=null}, self.allocator);
        // NOTE: want Literal and Identifiers to persist outside scope, so we need to heap allocate
        if (self.match(2, [2]TokenType{TokenType.NUMBER, TokenType.STRING})) return expr.Literal.new(self.previous().literal, self.allocator);

        if (self.match(1, [1]TokenType{TokenType.IDENTIFIER})) return expr.Var.new(self.previous(), self.allocator);
        
        if (self.match(1, [1]TokenType{TokenType.LEFT_PAREN})) {
            const e = try self.expression();
            _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
            return expr.Grouping.new(e, self.allocator);
        }
        return Error.ParseError;
    }

    // SYNCHRONIZER FOR RECURSIVE DESCENT PARSER
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

    // ERROR HANDLER
    fn consume(self: *Parser, ttype: TokenType, message: []const u8) Error!Token {
        if (self.check(ttype)) return self.advance();

        const error_token = self.peek();
        if (error_token.ttype == TokenType.EOF) {
            std.debug.print("[line: {d}, near end] {s}\n",.{error_token.line, message});
        } else {
            std.debug.print("[line: {d}, near '{s}'] {s}\n",.{error_token.line, error_token.lexeme, message});
        }
        return Error.ParseError;
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

    // TODO: double-check logic for freeing! Need a better understanding of memory
    // pub fn deinit(self: *Parser) void {
    //     self.allocator.free(self.tokens);
    // }
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

const std = @import("std");

const ArrayList = std.ArrayList;

const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const LiteralValue = @import("token/literal.zig").Literal;

const expr = @import("expr/expr.zig");

const Stmt = @import("stmt/stmt.zig").Stmt;
const s = @import("stmt/stmt.zig");

const Error = @import("error.zig").Error;

// TODO: Add more granular errors for ParserErrors
// TODO: make these arrays evaluated at compile time since we know these are FIXED sizes
const equality_match = [2]TokenType{TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL};
const unary_match = [2]TokenType{TokenType.BANG, TokenType.MINUS};
const factor_match = [2]TokenType{TokenType.SLASH, TokenType.STAR};
const comparison_match = [4]TokenType{TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL};
const term_match = [2]TokenType{TokenType.MINUS, TokenType.PLUS};
const var_match = [1]TokenType{TokenType.VAR};

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
    // declaration    -> varDecl 
    //                   | statement ;
    // statement      -> exprStmt
    //                   | ifStmt
    //                   | printStmt
    //                   | block ;
    // ifStmt         -> if" "(" expression ")" statement
    //                   ( "else" statement )? ;  
    // block          -> "{" declaration* "}" ;
    //
    
    fn declaration(self: *Parser) Error!Stmt {
        try {
            if (self.match(var_match.len, var_match)) return try self.varDeclaration();
            return try self.statement();
        } catch {
            self.synchronize();
            return Error.ParseError;
        };
    }

    fn varDeclaration(self: *Parser) Error!Stmt {
        const name = try self.consume(TokenType.IDENTIFIER, "Expect variable name");
        var initializer: ?*const expr.Expr = null; 

        // NOTE: there may some issues with the case of variable declaration v. initialization
        if (self.match(1, [1]TokenType{TokenType.EQUAL})) initializer = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration");
        return s.Var.new(name, initializer);
    }

    fn statement(self: *Parser) Error!Stmt {
        if (self.match(1, [1]TokenType{TokenType.PRINT})) return self.printStatement();
        if (self.match(1, [1]TokenType{TokenType.LEFT_BRACE})) return s.Block.new(try self.block());
        return self.expressionStatement();
    }

    fn block(self: *Parser) Error!ArrayList(Stmt) {
        var statements = ArrayList(s.Stmt).init(self.allocator);
        while (!self.check(TokenType.RIGHT_BRACE) and !self.reachedEnd()) try statements.append(try self.declaration());
        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect ';' after block");
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
    //                   | equality ;
    // equality       -> comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison     -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term           -> factor ( ( "-" | "+" ) factor )* ;
    // factor         -> unary ( ( "/" | "*" ) unary )* ;
    // unary          -> ( "!" | "-" ) unary
    //                -> | primary ;
    // primary        -> NUMBER | STRING | "true" | "false" | "nil"
    //                   | "(" expression ")" ;

    fn expression(self: *Parser) Error!*const expr.Expr {
        return try self.assignment();
    }

    fn assignment(self: *Parser) Error!*const expr.Expr {
        // First naively evaluate the token before the '='
        const greedy_expr = try self.equality();
        var equals: Token = undefined;
        var value: *const expr.Expr = undefined;

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

    fn equality(self: *Parser) Error!*const expr.Expr {
        var e = try self.comparison();
        while (self.match(equality_match.len, equality_match)) {
            const operator = self.previous();
            const right = try self.comparison();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn comparison(self: *Parser) Error!*const expr.Expr {
        var e = try self.term();
        while (self.match(comparison_match.len, comparison_match)) {
            const operator = self.previous();
            const right = try self.term();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn term(self: *Parser) Error!*const expr.Expr {
        var e = try self.factor();
        while (self.match(term_match.len, term_match)) {
            const operator = self.previous();
            const right = try self.factor();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn factor(self: *Parser) Error!*const expr.Expr {
        var e = try self.unary();
        while (self.match(factor_match.len, factor_match)) {
            const operator = self.previous();
            const right = try self.unary();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn unary(self: *Parser) Error!*const expr.Expr {
        if (self.match(unary_match.len, unary_match)) {
            const operator = self.previous();
            const right = try self.unary();
            return expr.Unary.new(operator, right, self.allocator);
        }
        return try self.primary();
    }

    fn primary(self: *Parser) Error!*const expr.Expr {
        if (self.match(1, [1]TokenType{TokenType.FALSE})) return expr.Literal.new(LiteralValue{.Bool=false}, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.TRUE})) return expr.Literal.new(LiteralValue{.Bool=true}, self.allocator);
        if (self.match(1, [1]TokenType{TokenType.NIL})) return expr.Literal.new(LiteralValue{.Nil=null}, self.allocator);
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

test "private_functionality" {
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

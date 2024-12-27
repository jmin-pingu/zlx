const std = @import("std");
const Error = @import("error.zig").Error;
const Token = @import("token/token.zig").Token;
const LiteralValue = @import("token/token.zig").LiteralValue;
const TokenType = @import("token/token_type.zig").TokenType;
const expr = @import("expr.zig");

const ArrayList = std.ArrayList;

// TODO: Add more granular errors for ParserErrors
var equality_match = [_]TokenType{TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL};
var unary_match = [_]TokenType{TokenType.BANG, TokenType.MINUS};
var factor_match = [_]TokenType{TokenType.SLASH, TokenType.STAR};
var comparison_match = [_]TokenType{TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL};
var term_match = [_]TokenType{TokenType.MINUS, TokenType.PLUS};


pub const Parser = struct {
    tokens: ArrayList(Token),
    current: u64 = 0,
    allocator: std.mem.Allocator,

    pub fn new(tokens: ArrayList(Token), allocator: std.mem.Allocator) Parser {
        return Parser{ .tokens = tokens, .allocator = allocator};
    }

    pub fn parse(self: *Parser) Error!*const expr.Expr {
        return self.expression() catch return Error.ParseError; //std.debug.panic("Panicked due to Parse Error");
    }

    // GRAMMAR RULES
    fn expression(self: *Parser) Error!*const expr.Expr {
        return try self.equality();
    }

    fn equality(self: *Parser) Error!*const expr.Expr {
        var e = try self.comparison();
        while (self.match(&equality_match)) {
            const operator = self.previous();
            const right = try self.comparison();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn comparison(self: *Parser) Error!*const expr.Expr {
        var e = try self.term();
        while (self.match(&comparison_match)) {
            const operator = self.previous();
            const right = try self.term();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn term(self: *Parser) Error!*const expr.Expr {
        var e = try self.factor();
        while (self.match(&term_match)) {
            const operator = self.previous();
            const right = try self.factor();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn factor(self: *Parser) Error!*const expr.Expr {
        var e = try self.unary();
        while (self.match(&factor_match)) {
            const operator = self.previous();
            const right = try self.unary();
            e = expr.Binary.new(e, operator, right, self.allocator);
        }
        return e;
    }

    fn unary(self: *Parser) Error!*const expr.Expr {
        if (self.match(&unary_match)) {
            const operator = self.previous();
            const right = try self.unary();
            return expr.Unary.new(operator, right, self.allocator);
        }
        return try self.primary();
    }

    fn primary(self: *Parser) Error!*const expr.Expr {
        var false_match = [_]TokenType{TokenType.FALSE};
        var true_match = [_]TokenType{TokenType.TRUE};
        var nil_match = [_]TokenType{TokenType.NIL};
        var literal_match = [_]TokenType{TokenType.NUMBER, TokenType.STRING};
        var paren_match = [_]TokenType{TokenType.LEFT_PAREN};

        if (self.match(&false_match)) return expr.Literal.new(LiteralValue{.Bool=false}, self.allocator);
        if (self.match(&true_match)) return expr.Literal.new(LiteralValue{.Bool=true}, self.allocator);
        if (self.match(&nil_match)) return expr.Literal.new(LiteralValue{.Nil=null}, self.allocator);

        if (self.match(&literal_match)) return expr.Literal.new(self.previous().literal, self.allocator);
        
        if (self.match(&paren_match)) {
            const e = try self.expression();
            _ = try self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
            return expr.Grouping.new(e, self.allocator);
        }
        return Error.ParseError;
    }

    // SYNCHRONIZER FOR RECURSIVE DESCENT PARSER
    fn synchronize(self: *Parser) void {
        self.advance();
        while (!self.reached_end()) {
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
            std.debug.print("{d} at end{s}",.{error_token.line, message});
        } else {
            std.debug.print("{d} at '{s}' {s}",.{error_token.line, error_token.lexeme, message});
        }
        return Error.ParseError;
    }
    
    // HELPER FUNCTIONS
    fn match(self: *Parser, types: []TokenType) bool { 
        for (types) |ttype| {
            if (self.check(ttype)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }
 
    fn check(self: *Parser, ttype: TokenType) bool { 
        if (self.reached_end()) return false;
        return self.peek().ttype == ttype;
    }

    fn advance(self: *Parser) Token { 
        if (!self.reached_end()) self.current += 1;
        return self.previous();
    }

    fn reached_end(self: *Parser) bool { 
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
    try std.testing.expectEqual(false, parser.reached_end());
    
    // Take a step
    try std.testing.expectEqual(Token.new(TokenType.NUMBER, "1", "1", 1), parser.advance());
    try std.testing.expectEqual(Token.new(TokenType.NUMBER, "1", "1", 1), parser.previous());
    try std.testing.expectEqual(Token.new(TokenType.STAR, "*", null, 1), parser.peek());
    try std.testing.expectEqual(false, parser.reached_end());

    // Take another step
    try std.testing.expectEqual(Token.new(TokenType.STAR, "*", null, 1), parser.advance());
    try std.testing.expectEqual(Token.new(TokenType.STAR, "*", null, 1), parser.previous());
    try std.testing.expectEqual(Token.new(TokenType.NUMBER, "2", "2", 1), parser.peek());
    try std.testing.expectEqual(false, parser.reached_end());

    // Step til end 
    _ = parser.advance();
    try std.testing.expectEqual(true, parser.reached_end());
}

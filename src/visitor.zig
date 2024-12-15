const std = @import("std");
const expr = @import("expr.zig");
const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;

pub const Visitor = struct {
    expression: expr.Expr,

    pub fn new(expression: expr.Expr) Visitor {
        return Visitor{ .expression = expression};
    }
    
    pub fn visit(self: *Visitor) u32 { 
        switch (self.expression.dtype) {
            .Binary => {return 1;},
            .Grouping => {return 2;},
            .Literal => {return 3;},
            .Unary => {return 4;},
        }
    }

    // pub fn print(self: *Visitor) { 

    // }
};

test "instantiation_test" {
    const expression = expr.Expr.new(
        expr.ExprType{ .Binary = expr.Binary.new(
            &expr.Expr.new(
                expr.ExprType{.Literal=expr.Literal.new("10")}
            ),
            Token.new(TokenType.STAR, "*", null, 1),
            &expr.Expr.new(
                expr.ExprType{.Literal=expr.Literal.new("11")}
            ),
        )}
    );
    // var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    // defer arena.deinit();
    // const allocator = arena.allocator();
    var visitor = Visitor.new(expression);
    const out = visitor.visit();
    try std.testing.expectEqual(@as(u32, 1), out);
}

test "visit_test" {
    const expression = expr.Expr.new(
        expr.ExprType{ .Binary = expr.Binary.new(
            &expr.Expr.new(expr.ExprType{.Unary=expr.Unary.new(
                Token.new()
            )}),
            Token.new(TokenType.STAR, "*", null, 1),
            &expr.Expr.new(expr.ExprType{.Grouping=expr.Grouping.new(
                &expr.Expr.new(
                    expr.ExprType{}Literal.new("45.67")
                )
            )}),
        )}
    );
    // var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    // defer arena.deinit();
    // const allocator = arena.allocator();
    var visitor = Visitor.new(expression);
    const out = visitor.visit();
    try std.testing.expectEqual(@as(u32, 1), out);
}

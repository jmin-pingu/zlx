const std = @import("std");
const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;

pub const Expr = struct {
    dtype: type 

    pub fn new(dtype: type) Expr {
        return Expr{ .dtype = dtype };
    }
};

pub const {s} = struct {
    {s}: {s},
    {s}: {s},
    {s}: {s},

    pub fn new({}: {}) {s} {
        return {s};
    }
};

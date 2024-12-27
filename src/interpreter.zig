const std = @import("std");
const expr = @import("expr.zig");
const Token = @import("token/token.zig").Token;
const TokenType = @import("token/token_type.zig").TokenType;
const Error = @import("error.zig").Error;

const Type = union{
    u8, 
    []*const u8,
}


const Token = @import("scanner.zig").Token;
const std = @import("std");

pub const Mode = enum {
    Debug,
    Default
};

pub const Error = ParseError || EncodingError;

pub const EncodingError = error {
    InvalidState,
} || GenericError;

pub const ParseError = error {
    PrefixRuleUndefined,
    UnaryOperatorUndefined,
    BinaryOperatorUndefined,
    ChunkWriteError,
    ExpectEOF,
    NoClosingRightParenthesis,
    NoClosingSemicolon,
    StringToFloatParseError,
    ChunkConstantsOverflow,
    ConsumedErrorToken,
    ConsumedTokenMismatch,
    LiteralUndefined,
    StringUndefined,
    VarMissingIdentifier,
    InvalidAssignmentTarget,
    NoClosingRightBrace,
    DuplicateIdentifierInScope,
    ConstNotDefined,
    ConstIsImmutable,
    LocalVariableShadowing,
    ExpectLeftParenthesisAfterIf,
    ExpectRightParenthesisAfterIf,
    JumpTooLarge,
    ExpectLeftParenthesisAfterWhile,
    ExpectRightParenthesisAfterWhile,
    ExpectLeftParenthesisAfterFor,
    ExpectRightParenthesisAfterFor,
    ExpectLeftParenthesisAfterSwitch,
    ExpectRightParenthesisAfterSwitch,
    ExpectLeftBraceAfterSwitch,
    ExpectRightBraceAfterSwitch,
    ExpectArrowAfterSwitchArm,
    ExpectSemicolonAfterFor,
    DuplicateDefaultInSwitchScope,
    ContinueNotNestedWithinLoop,
} || GenericError || ScanError;

pub const ScanError = error {
    UnterminatedString,
} || GenericError;

const GenericError = error {
    OutOfIndex
} || std.mem.Allocator.Error;

pub fn errorAt(token: *const Token, err: ParseError) ParseError!void {
    std.debug.print("[line {d}] Error", .{token.line});
    switch (token.ttype) {
        .EOF => std.debug.print(" at end", .{}),
        .ERROR => {},
        else => std.debug.print(" at `{s}`", .{token.token}),
    }

    const maybeMessage = switch (err) {
        ParseError.ExpectEOF =>  "Expected end of expression",
        ParseError.NoClosingRightParenthesis => "Expect ')' after expression.",
        ParseError.StringToFloatParseError => "String could not be parsed to a value (float).",
        ParseError.ChunkConstantsOverflow => "Too many constants in one chunk.",
        ParseError.InvalidAssignmentTarget => "Invalid assignment target.",
        ParseError.NoClosingRightBrace => "No closing right brace.",
        else => null,
    };
    if (maybeMessage) |message| {
        std.debug.print(": {s}\n", .{message}); 
    } else {
        std.debug.print("\n", .{}); 
    }
    return err;
}

pub fn debug(comptime fmt: []const u8, args: anytype, mode: Mode) void {
    if (mode == .Debug) std.debug.print(fmt, args);
}


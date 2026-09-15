const Token = @import("scanner.zig").Token;
const std = @import("std");

pub const Mode = enum {
    Debug,
    DebugGC,
    StressGC,
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
    ExpectLeftParenthesisAfterFnName,
    ExpectRightParenthesisAfterFnName,
    ExpectLeftBraceAfterFnBody,
    FunctionParameterOverflow,
    ReturnAtTopLevelCode,
    ClosureVariableOverflow,
    TODO
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
        ParseError.ReturnAtTopLevelCode => "Cannot return from top-level code.",
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


const testing = std.testing;

test "errorAt returns the error it was given" {
    const tok = Token.init(.IDENTIFIER, "foo", 3);
    try testing.expectError(ParseError.InvalidAssignmentTarget, errorAt(&tok, ParseError.InvalidAssignmentTarget));
    try testing.expectError(ParseError.TODO, errorAt(&tok, ParseError.TODO));
}

test "errorAt handles EOF and ERROR tokens" {
    const eof = Token.init(.EOF, "EOF", 1);
    try testing.expectError(ParseError.ExpectEOF, errorAt(&eof, ParseError.ExpectEOF));
    const bad = Token.errorToken("unused", 2);
    try testing.expectError(ParseError.ConsumedErrorToken, errorAt(&bad, ParseError.ConsumedErrorToken));
}

test "error sets nest into the umbrella Error set" {
    const scan: ParseError = ScanError.UnterminatedString;
    const oob: ParseError = error.OutOfIndex;
    const oom: ParseError = error.OutOfMemory;
    const parse: Error = ParseError.NoClosingSemicolon;
    const enc: Error = EncodingError.InvalidState;
    try testing.expectEqual(error.UnterminatedString, scan);
    try testing.expectEqual(error.OutOfIndex, oob);
    try testing.expectEqual(error.OutOfMemory, oom);
    try testing.expectEqual(error.NoClosingSemicolon, parse);
    try testing.expectEqual(error.InvalidState, enc);
}

test "debug only prints in Debug mode" {
    debug("never printed {d}\n", .{1}, .Default);
    debug("never printed {d}\n", .{1}, .DebugGC);
    debug("never printed {d}\n", .{1}, .StressGC);
    debug("printed in debug {d}\n", .{1}, .Debug);
}

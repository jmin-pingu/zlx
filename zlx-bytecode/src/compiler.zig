const std = @import("std");
const TokenType = @import("scanner.zig").TokenType;
const Token = @import("scanner.zig").Token;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const String = @import("value.zig").String;
const Object = @import("value.zig").Object;
const Function = @import("value.zig").Function;
const FunctionType = @import("value.zig").FunctionType;
const ParseError = @import("error.zig").ParseError;
const Parser = @import("parser.zig").Parser;
const errorAt = @import("error.zig").errorAt;
const debug = @import("error.zig").debug;
const assert = std.debug.assert;
const mode = @import("main.zig").mode;
const Metadata = @import("gc.zig").Metadata;

// TODO: need better error handling for this entire project
const Precedence = enum {
    NONE, 
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY 
};

pub const Compiler = struct {
    parser: *Parser, 
    metadata: *Metadata,
    function: *Function,
    functionType: FunctionType,
    locals: [std.math.maxInt(u8)+1]Local,
    localCount: u8,
    scopeDepth: u8,
    currentLoop: ?usize,


    const Self = @This();
    const ParseFn = *const fn(*Compiler, canAssign: bool, allocator: std.mem.Allocator) ParseError!void;
    const ParseRule = struct {
        prefix: ?ParseFn,
        infix: ?ParseFn, 
        precedence: Precedence, 

        pub fn init(prefix: ?ParseFn, infix: ?ParseFn, precedence: Precedence) ParseRule {
            return .{
                .prefix = prefix,
                .infix = infix, 
                .precedence = precedence,
            };
        }
    };

    const Local = struct {
        name: Token,
        depth: ?u8,
        mutable: bool,
    };

    // Parse Table for Pratt's Top Down Parsing Algorithm
    var parseTable = std.enums.EnumArray(TokenType, ParseRule).init(
        .{
            .LEFT_PAREN     = ParseRule.init(Self.grouping, Self.call, .CALL),
            .RIGHT_PAREN    = ParseRule.init(null, null, .NONE),
            .LEFT_BRACE     = ParseRule.init(null, null, .NONE),
            .RIGHT_BRACE    = ParseRule.init(null, null, .NONE),
            .COMMA          = ParseRule.init(null, null, .NONE),
            .DOT            = ParseRule.init(null, null, .NONE),
            .MINUS          = ParseRule.init(Self.unary, Self.binary, .TERM),
            .PLUS           = ParseRule.init(null, Self.binary, .TERM),
            .SEMICOLON      = ParseRule.init(null, null, .NONE),
            .SLASH          = ParseRule.init(null, Self.binary, .FACTOR),
            .STAR           = ParseRule.init(null, Self.binary, .FACTOR),
            .BANG           = ParseRule.init(Self.unary, null, .NONE),
            .BANG_EQUAL     = ParseRule.init(null, Self.binary, .EQUALITY),
            .EQUAL          = ParseRule.init(null, null, .NONE),
            .EQUAL_EQUAL    = ParseRule.init(null, Self.binary, .EQUALITY),
            .GREATER        = ParseRule.init(null, Self.binary, .COMPARISON),
            .GREATER_EQUAL  = ParseRule.init(null, Self.binary, .COMPARISON),
            .LESS           = ParseRule.init(null, Self.binary, .COMPARISON),
            .LESS_EQUAL     = ParseRule.init(null, Self.binary, .COMPARISON),
            .IDENTIFIER     = ParseRule.init(Self.variable, null, .NONE),
            .STRING         = ParseRule.init(Self.string, null, .NONE),
            .NUMBER         = ParseRule.init(Self.number, null, .NONE),
            .AND            = ParseRule.init(null, Self.logicalAnd, .AND),
            .CLASS          = ParseRule.init(null, null, .NONE),
            .ELSE           = ParseRule.init(null, null, .NONE),
            .FOR            = ParseRule.init(null, null, .NONE),
            .FUN            = ParseRule.init(null, null, .NONE),
            .IF             = ParseRule.init(null, null, .NONE),
            .NIL            = ParseRule.init(Self.literal, null, .NONE),
            .OR             = ParseRule.init(null, Self.logicalOr, .OR),
            .PRINT          = ParseRule.init(null, null, .NONE),
            .SUPER          = ParseRule.init(null, null, .NONE),
            .THIS           = ParseRule.init(null, null, .NONE),
            .TRUE           = ParseRule.init(Self.literal, null, .NONE),
            .FALSE          = ParseRule.init(Self.literal, null, .NONE),
            .RETURN         = ParseRule.init(null, null, .NONE),
            .VAR            = ParseRule.init(null, null, .NONE),
            .CONST          = ParseRule.init(null, null, .NONE),
            .WHILE          = ParseRule.init(null, null, .NONE),
            .SWITCH         = ParseRule.init(null, null, .NONE),
            .CONTINUE       = ParseRule.init(null, null, .NONE),
            .DEFAULT        = ParseRule.init(null, null, .NONE),
            .ERROR          = ParseRule.init(null, null, .NONE),
            .ARROW          = ParseRule.init(null, null, .NONE),
            .EOF            = ParseRule.init(null, null, .NONE),
        }
    );

    pub fn init(metadata: *Metadata, ftype: FunctionType, maybeParser: ?*Parser, allocator: std.mem.Allocator) !Self {
        const initLocal = Local{
            .depth=0,
            .name=Token.init(.IDENTIFIER, "", 0),
            .mutable=false,
        };
        const locals = [1]Local{initLocal} ++ [_]Local{undefined} ** (std.math.maxInt(u8));
        const name = switch(ftype) {
            .Function => out: {
                if (maybeParser) |parser| {
                    break :out try String.initString(parser.previous.token, metadata, allocator);
                } else {
                    return ParseError.TODO;
                }
            },
            .Script => null,
        };
        return .{
            .parser = out: {
                if (maybeParser) |parser| {
                    break :out parser;
                } else {
                    break :out undefined;
                }
            },
            .metadata = metadata, 
            .locals = locals,
            .localCount = 1,
            .scopeDepth = 0,
            .currentLoop = null,
            .function = try Function.initFunction(allocator, metadata, name),
            .functionType = ftype,
        };
    }

    pub fn currentChunk(self: *Self) *Chunk {
        return self.function.chunk;
    }


    pub fn compile(self: *Self, source: []const u8, allocator: std.mem.Allocator) !*Function {
        var parser = try Parser.init(source, allocator);
        self.parser = &parser;
        try self.parser.advance();
        while (!(try self.match(.EOF))) {
            try self.declaration(allocator);
        }
        return try self.endCompiler(allocator);
    }

    pub fn endCompiler(self: *Self, allocator: std.mem.Allocator) !*Function {
        try self.emitReturn(allocator);
        // NOTE: need to disassemble 
        return self.function;
    }

    fn declaration(self: *Self, allocator: std.mem.Allocator) ParseError!void {
        if (try self.match(.FUN)) {
            try self.funDeclaration(allocator);
        } else if (try self.match(.VAR)) {
            try self.varDeclaration(allocator, true);
        } else if (try self.match(.CONST)) {
            try self.varDeclaration(allocator, false);
        } else {
            self.statement(allocator) catch |err| {
                std.debug.print("error: {any}\n", .{err});
                try self.synchronize();
            };
        }
    }

    fn beginScope(self: *Self) void {
        self.scopeDepth += 1;
    }

    fn endScope(self: *Self, allocator: std.mem.Allocator) !void {
        self.scopeDepth -= 1;
        while (self.localCount > 0 and self.locals[self.localCount-1].depth.? > self.scopeDepth) {
            try self.emitByte(@intFromEnum(OpCode.OP_POP), allocator);
            self.localCount -= 1;
        }
    }

    fn block(self: *Self, allocator: std.mem.Allocator) !void {
        while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
            try self.declaration(allocator);
        }

        try self.parser.consume(.RIGHT_BRACE, ParseError.NoClosingRightBrace);
    }

    fn funDeclaration(self: *Self, allocator: std.mem.Allocator) !void {
        const global = try self.parseVariable(allocator, false);
        self.markInitialized();
        try self.compileFunction(.Function, allocator);
        try self.defineVariable(global, allocator);
    }

    fn compileFunction(self: *Self, ftype: FunctionType, allocator: std.mem.Allocator) !void {
        const nestedCompiler = try allocator.create(Compiler);
        defer allocator.destroy(nestedCompiler);
        nestedCompiler.* = try Compiler.init(self.metadata, ftype, self.parser, allocator);
        nestedCompiler.beginScope();

        try nestedCompiler.parser.consume(.LEFT_PAREN, ParseError.ExpectLeftParenthesisAfterFnName);
        if (!nestedCompiler.check(.RIGHT_PAREN)) {
            while (true) {
                nestedCompiler.function.arity += 1;
                if (nestedCompiler.function.arity > 255) {
                    try errorAt(&nestedCompiler.parser.current, ParseError.FunctionParameterOverflow);
                }
                const constant = try nestedCompiler.parseVariable(allocator, false);
                try nestedCompiler.defineVariable(constant, allocator);
                if (!(try nestedCompiler.match(.COMMA))) {
                    break;
                }
            }

        }
        try nestedCompiler.parser.consume(.RIGHT_PAREN, ParseError.ExpectRightParenthesisAfterFnName);

        try nestedCompiler.parser.consume(.LEFT_BRACE, ParseError.ExpectLeftBraceAfterFnBody);
        try nestedCompiler.block(allocator);

        const function = try nestedCompiler.endCompiler(allocator);
        try self.emitBytes(
            @intFromEnum(OpCode.OP_CONSTANT), 
            try self.makeConstant(try Value.initFunction(function), allocator), 
            allocator
        );
    }

    fn printLocals(self: Self) void {
        std.debug.print("[ ", .{});
        for (self.locals[0..self.localCount]) |local| {
            std.debug.print("({s}, {?d}), ", .{local.name.token, local.depth});
        }
        std.debug.print("]\n ", .{});
    }

    fn varDeclaration(self: *Self, allocator: std.mem.Allocator, isMutable: bool) !void {
        const global = try self.parseVariable(allocator, isMutable);
        if (try self.match(.EQUAL)) {
            try self.expression(allocator);
        } else {
            if (isMutable) {
                try self.emitByte(@intFromEnum(OpCode.OP_NIL), allocator);
            } else {
                // NOTE: maybe throw a compile time error instead
                return ParseError.ConstNotDefined;
            }
        }
        try self.parser.consume(.SEMICOLON, ParseError.NoClosingSemicolon);
        try self.defineVariable(global, allocator);
    }
    
    fn parseVariable(self: *Self, allocator: std.mem.Allocator, isMutable: bool) !u8 {
        try self.parser.consume(.IDENTIFIER, ParseError.VarMissingIdentifier);
        try self.declareVariable(isMutable);
        if (self.scopeDepth > 0) return 0;
        try self.metadata.addGlobal(self.parser.previous.token, isMutable);
        return try self.identifierConstant(self.parser.previous.token, allocator);
    }

    fn identifierConstant(self: *Self, name: []const u8, allocator: std.mem.Allocator) !u8 {
        const identifier = try Value.initString(name, self.metadata, allocator);
        return try self.makeConstant(identifier, allocator);
    }

    fn declareVariable(self: *Self, isMutable: bool) !void {
        const name = self.parser.previous;
        if (self.scopeDepth == 0) {
            if (self.metadata.isGlobal(name.token)) return ParseError.DuplicateIdentifierInScope;
            return;
        }

        var d = Decrementer.init(self.localCount, 0, -1);
        while (d.next()) |idx| {
            const local = self.locals[@intCast(idx)];
            if (local.depth != null and local.depth.? < self.scopeDepth) {
                break;
            }

            if (self.identifiersEqual(name, local.name)) return ParseError.DuplicateIdentifierInScope;
        }
        self.addLocal(name, isMutable);
    }

    fn identifiersEqual(self: *Self, a: Token, b: Token) bool {
        _ = self;
        if (a.token.len != b.token.len) return false;
        return std.mem.eql(u8, a.token, b.token);
    }

    fn addLocal(self: *Self, name: Token, isMutable: bool) void {
        assert(self.localCount < self.locals.len);
        self.locals[self.localCount] = .{ .name = name, .depth = null, .mutable = isMutable };
        self.localCount += 1;
    }

    fn defineVariable(self: *Self, global: u8, allocator: std.mem.Allocator) !void {
        if (self.scopeDepth > 0) {
            self.markInitialized();
            return;
        }
        // NOTE: change op_code
        try self.emitBytes(@intFromEnum(OpCode.OP_DEFINE_GLOBAL), global, allocator);
    }

    fn markInitialized(self: *Self) void {
        if (self.scopeDepth == 0) return;
        self.locals[self.localCount-1].depth = self.scopeDepth;
    }

    fn statement(self: *Self, allocator: std.mem.Allocator) ParseError!void {
        if (try self.match(.PRINT)) {
            try self.printStatement(allocator);
        } else if (try self.match(.CONTINUE)) {
            try self.continueStatement(allocator);
        } else if (try self.match(.IF)) {
            try self.ifStatement(allocator);
        } else if (try self.match(.SWITCH)) {
            try self.switchStatement(allocator);
        } else if (try self.match(.FOR)) {
            try self.forStatement(allocator);
        } else if (try self.match(.WHILE)) {
            try self.whileStatement(allocator);
        } else if (try self.match(.LEFT_BRACE)) {
            self.beginScope(); 
            try self.block(allocator);
            try self.endScope(allocator); 
        } else {
            try self.expressionStatement(allocator);
        }
    }

    fn synchronize(self: *Self) !void {
        while (self.parser.current.ttype != .EOF) {
            if (self.parser.previous.ttype != .SEMICOLON) return;
            switch (self.parser.current.ttype) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {}
            }
            try self.parser.advance();
        }
    }

    // Statements >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    fn printStatement(self: *Self, allocator: std.mem.Allocator) !void {
        try self.expression(allocator);
        try self.parser.consume(.SEMICOLON, ParseError.NoClosingSemicolon);
        try self.emitByte(OpCode.OP_PRINT.asByte(), allocator);
    }

    fn continueStatement(self: *Self, allocator: std.mem.Allocator) !void {
        if (self.currentLoop) |loopStart| {
            try self.emitLoop(loopStart, allocator);
            try self.parser.consume(.SEMICOLON, ParseError.NoClosingSemicolon);
        } else {
            return ParseError.ContinueNotNestedWithinLoop;
        }
    }

    fn forStatement(self: *Self, allocator: std.mem.Allocator) !void {
        self.beginScope();
        try self.parser.consume(.LEFT_PAREN, ParseError.ExpectLeftParenthesisAfterFor);

        // Initializer
        if (try self.match(.SEMICOLON)) {
        } else if (try self.match(.VAR)) {
            try self.varDeclaration(allocator, true);
        } else {
            try self.expressionStatement(allocator);
        }
        var loopStart: usize = self.currentChunk().indexOfNextInstruction();
        self.currentLoop = loopStart;
        defer self.currentLoop = undefined;
        var exitJump: ?usize = null;
        
        // Optional Conditional
        if (!(try self.match(.SEMICOLON))) {
            try self.expression(allocator);
            try self.parser.consume(.SEMICOLON, ParseError.ExpectSemicolonAfterFor);
            exitJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE.asByte(), allocator);
            try self.emitByte(OpCode.OP_POP.asByte(), allocator);
        }

        // Optional Expression
        if (!(try self.match(.RIGHT_PAREN))) {
            const bodyJump = try self.emitJump(OpCode.OP_JUMP.asByte(), allocator);
            const incrementStart = self.currentChunk().indexOfNextInstruction();
            try self.expression(allocator);
            try self.emitByte(OpCode.OP_POP.asByte(), allocator);
            try self.parser.consume(.RIGHT_PAREN, ParseError.ExpectRightParenthesisAfterFor);

            try self.emitLoop(loopStart, allocator);
            loopStart = incrementStart;
            try self.patchJump(bodyJump, allocator);
        }

        try self.statement(allocator);
        try self.emitLoop(loopStart, allocator);
        if (exitJump != null) {
            try self.patchJump(exitJump.?, allocator);
            try self.emitByte(OpCode.OP_POP.asByte(), allocator);
        }
        try self.endScope(allocator);
    }

    fn whileStatement(self: *Self, allocator: std.mem.Allocator) !void {
        const loopStart = self.currentChunk().indexOfNextInstruction();
        self.currentLoop = loopStart;
        defer self.currentLoop = undefined;

        try self.parser.consume(.LEFT_PAREN, ParseError.ExpectLeftParenthesisAfterWhile);
        try self.expression(allocator);
        try self.parser.consume(.RIGHT_PAREN, ParseError.ExpectRightParenthesisAfterWhile);

        const exitJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE.asByte(), allocator);
        try self.emitByte(OpCode.OP_POP.asByte(), allocator);
        try self.statement(allocator);
        try self.emitLoop(loopStart, allocator);

        try self.patchJump(exitJump, allocator);
        try self.emitByte(OpCode.OP_POP.asByte(), allocator);
    }

    // TODO: clean up switchStatement logic
    // TODO: add comprehensive type checking + pattern matching syntax
    fn switchStatement(self: *Self, allocator: std.mem.Allocator) !void {
        try self.parser.consume(.LEFT_PAREN, ParseError.ExpectLeftParenthesisAfterSwitch);
        try self.expression(allocator);
        try self.parser.consume(.RIGHT_PAREN, ParseError.ExpectRightParenthesisAfterSwitch);

        try self.parser.consume(.LEFT_BRACE, ParseError.ExpectLeftBraceAfterSwitch);
        try self.consumeSwitchArms(allocator);
    }

    fn consumeSwitchArms(self: *Self, allocator: std.mem.Allocator) !void {
        // Everything should jump to default
        var armSuccessJumps: std.ArrayList(usize) = .empty;
        var armJump: ?usize = null;
        var defaultStart: ?usize = null;
        while (!(try self.match(.RIGHT_BRACE))) {
            if (!(try self.match(.DEFAULT))) {
                try self.expression(allocator);
                try self.emitByte(OpCode.OP_EQUAL_INPLACE.asByte(), allocator);
                armJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE.asByte(), allocator);
                try self.emitByte(OpCode.OP_POP.asByte(), allocator);
                try self.consumeSwitchArmStatement(&armSuccessJumps, allocator);
                try self.patchJump(armJump.?, allocator);
                try self.emitByte(OpCode.OP_POP.asByte(), allocator);
            } else {
                if (defaultStart != null) return ParseError.DuplicateDefaultInSwitchScope;
                defaultStart = self.currentChunk().indexOfNextInstruction();
                try self.consumeSwitchArmStatement(&armSuccessJumps, allocator);
            }
        } 

        if (armJump) |jump| try self.patchJump(jump, allocator);
        if (defaultStart) |start| {
            try self.emitByte(OpCode.OP_POP.asByte(), allocator);
            try self.emitLoop(start, allocator);
        } 

        for (armSuccessJumps.items) |jump| {
            try self.patchJump(jump, allocator);
        }
        try self.emitByte(OpCode.OP_POP.asByte(), allocator);
    }

    fn consumeSwitchArmStatement(self: *Self, successJumps: *std.ArrayList(usize), allocator: std.mem.Allocator) !void {
        try self.parser.consume(.ARROW, ParseError.ExpectArrowAfterSwitchArm);
        try self.statement(allocator);
        try successJumps.append(allocator, try self.emitJump(OpCode.OP_JUMP.asByte(), allocator));
    }


    fn ifStatement(self: *Self, allocator: std.mem.Allocator) !void {
        try self.parser.consume(.LEFT_PAREN, ParseError.ExpectLeftParenthesisAfterIf);
        try self.expression(allocator);
        try self.parser.consume(.RIGHT_PAREN, ParseError.ExpectRightParenthesisAfterIf);

        const thenJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE.asByte(), allocator);
        try self.emitByte(OpCode.OP_POP.asByte(), allocator);
        try self.statement(allocator);
        const elseJump = try self.emitJump(OpCode.OP_JUMP.asByte(), allocator);
        try self.patchJump(thenJump, allocator);
        try self.emitByte(OpCode.OP_POP.asByte(), allocator);

        if (try self.match(.ELSE)) try self.statement(allocator);
        try self.patchJump(elseJump, allocator);
    }

    fn expressionStatement(self: *Self, allocator: std.mem.Allocator) !void {
        try self.expression(allocator);
        try self.parser.consume(.SEMICOLON, ParseError.NoClosingSemicolon);
        try self.emitByte(OpCode.OP_POP.asByte(), allocator);
    }

    // Operator Precedence Parser Functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    fn expression(self: *Self, allocator: std.mem.Allocator) ParseError!void {
        try self.parsePrecedence(.ASSIGNMENT, allocator);
    }

    fn parsePrecedence(self: *Self, precedence: Precedence, allocator: std.mem.Allocator) ParseError!void {
        try self.parser.advance();
        const maybePrefixRule = self.getRule(self.parser.previous.ttype).prefix;
        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
        if (maybePrefixRule) |prefixRule| {
            try prefixRule(self, canAssign, allocator);
        } else {
            std.debug.print("{any}\n", .{self.parser.current});
            return ParseError.PrefixRuleUndefined;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(self.getRule(self.parser.current.ttype).precedence)) {
            try self.parser.advance();
            const maybeInfixRule = self.getRule(self.parser.previous.ttype).infix;
            if (maybeInfixRule) |infixRule| {
                try infixRule(self, canAssign, allocator);
            }       
        }

        if (canAssign and try self.match(.EQUAL)) {
            return errorAt(&self.parser.previous, ParseError.InvalidAssignmentTarget);
        }
    }

    fn getRule(self: *Self, ttype: TokenType) *const ParseRule {
        _ = self;
        return parseTable.getPtr(ttype);
    }

    fn patchJump(self: *Self, offset: usize, allocator: std.mem.Allocator) ParseError!void {
        const jump = self.currentChunk().indexOfLatestInstruction() - offset - 1;
        if (jump > std.math.maxInt(u16)) {
            return ParseError.JumpTooLarge;
        }

        // Store jump in big endian
        try self.currentChunk().code.replaceRange(allocator, offset, 2, &[_]u8{
            @truncate((jump >> 8) & 0xff),
            @truncate(jump & 0xff)
        });
    }

    // Code generation logic >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    fn emitByte(self: *Self, byte: u8, allocator: std.mem.Allocator) ParseError!void {
        self.currentChunk().write(byte, self.parser.previous.line, allocator) catch return ParseError.ChunkWriteError;
    }

    /// Emits a OP_LOOP followed by a short, representing the index to loop back to. 
    /// `loopStart`: the index of the instruction to loop back to
    fn emitLoop(self: *Self, loopStart: usize, allocator: std.mem.Allocator) ParseError!void {
        try self.emitByte(OpCode.OP_LOOP.asByte(), allocator);
        const offset = self.currentChunk().indexOfLatestInstruction() - loopStart + 3;
        if (offset > std.math.maxInt(u16)) {
            return ParseError.JumpTooLarge;
        }

        // Store jump in big endian
        try self.emitBytes(@truncate((offset >> 8) & 0xff), @truncate(offset & 0xff), allocator);
    }

    fn emitJump(self: *Self, byte: u8, allocator: std.mem.Allocator) ParseError!usize {
        try self.emitByte(byte, allocator);
        try self.emitByte(0xff, allocator);
        try self.emitByte(0xff, allocator);
        return self.currentChunk().indexOfLatestInstruction() - 1;
    }

    fn emitReturn(self: *Self, allocator: std.mem.Allocator) ParseError!void {
        try self.emitByte(OpCode.OP_RETURN.asByte(), allocator);
    }

    fn emitBytes(self: *Self, byte1: u8, byte2: u8, allocator: std.mem.Allocator) ParseError!void {
        try self.emitByte(byte1, allocator);
        try self.emitByte(byte2, allocator);
    }

    fn emitConstant(self: *Self, value: Value, allocator: std.mem.Allocator) ParseError!void {
        const constantIndex = try self.makeConstant(value, allocator);
        try self.emitBytes(
            OpCode.OP_CONSTANT.asByte(), 
            constantIndex,
            allocator
        );
    }

    fn makeConstant(self: *Self, value: Value, allocator: std.mem.Allocator) ParseError!u8 {
        const constantIdx = try self.currentChunk().addConstant(value, allocator);
        if (constantIdx > std.math.maxInt(u8)) {
            try self.parser.errorAtPrevious(ParseError.ChunkConstantsOverflow);
        }
        return constantIdx;
    }

    // Expression handling >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    fn unary(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        const operatorType = self.parser.previous.ttype;
        try self.parsePrecedence(.UNARY, allocator);

        return switch (operatorType) {
            .MINUS => self.emitByte(OpCode.OP_NEGATE.asByte(), allocator),
            .BANG => self.emitByte(OpCode.OP_NOT.asByte(), allocator),
            else => ParseError.UnaryOperatorUndefined
        };
    }

    fn literal(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        return switch (self.parser.previous.ttype) {
            .TRUE => self.emitByte(OpCode.OP_TRUE.asByte(), allocator),
            .FALSE => self.emitByte(OpCode.OP_FALSE.asByte(), allocator),
            .NIL => self.emitByte(OpCode.OP_NIL.asByte(), allocator),
            else => ParseError.LiteralUndefined
        };
    }

    fn logicalAnd(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        const endJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE.asByte(), allocator);
        try self.emitByte(OpCode.OP_POP.asByte(), allocator);
        try self.parsePrecedence(.AND, allocator);
        try self.patchJump(endJump, allocator);
    }

    fn logicalOr(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        const elseJump = try self.emitJump(OpCode.OP_JUMP_IF_FALSE.asByte(), allocator);
        const endJump = try self.emitJump(OpCode.OP_JUMP.asByte(), allocator);
        try self.patchJump(elseJump, allocator);
        try self.emitByte(OpCode.OP_POP.asByte(), allocator);

        try self.parsePrecedence(.OR, allocator);
        try self.patchJump(endJump, allocator);
    }

    // NOTE: need to add "metadata" for compiler .
    fn string(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        return switch (self.parser.previous.ttype) {
            .STRING => {
                const str = self.parser.previous.token;
                // NOTE: check if exists
                const stringLiteral = str[1..str.len-1];
                try self.emitConstant(
                    try Value.initString(stringLiteral, self.metadata, allocator), 
                    allocator
                );
            },
            else => ParseError.StringUndefined
        };
    }

    fn call(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        std.debug.print("call\n", .{});
        const argCount = try self.argumentList(allocator);
        try self.emitBytes(@intFromEnum(OpCode.OP_CALL), argCount, allocator);
    }

    fn argumentList(self: *Self, allocator: std.mem.Allocator) !u8 {
        var argCount: u8 = 0;
        if (!self.check(.RIGHT_PAREN)) {
            while (true) {
                try self.expression(allocator);
                if (argCount == 255) {
                    return ParseError.FunctionParameterOverflow;
                }
                argCount += 1;
                if (!(try self.match(.COMMA))) {
                    break;
                }
            }
        }

        try self.parser.consume(.RIGHT_PAREN, ParseError.TODO);
        return argCount;
    }

    fn grouping(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        try self.expression(allocator);
        return self.parser.consume(.RIGHT_PAREN, ParseError.NoClosingRightParenthesis);
    }

    fn variable(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        try self.namedVariable(self.parser.previous, canAssign, allocator);
    }


    fn namedVariable(self: *Self, name: Token, canAssign: bool, allocator: std.mem.Allocator) !void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;
        var arg: u8 = undefined;
        var isVar = true;
        const local = try self.resolveLocal(name);

        if (local == null) {
            arg = try self.identifierConstant(name.token, allocator);
            isVar = !self.metadata.isGlobalConst(name.token);
            getOp = .OP_GET_GLOBAL;
            setOp = .OP_SET_GLOBAL;
        } else {
            arg = local.?.index;
            isVar = local.?.mutable;
            getOp = .OP_GET_LOCAL;
            setOp = .OP_SET_LOCAL;
        }

        if (canAssign and try self.match(.EQUAL)) {
            if (isVar) {
                try self.expression(allocator);
                try self.emitBytes(@intFromEnum(setOp), arg, allocator);
            } else {
                return ParseError.ConstIsImmutable;
            }
        } else {
            try self.emitBytes(@intFromEnum(getOp), arg, allocator);
        }
    }

    fn resolveLocal(self: *Self, name: Token) !?struct { index: u8, mutable: bool } {
        var d = Decrementer.init(self.localCount, 0, -1);
        while (d.next()) |idx| {
            const local = self.locals[@intCast(idx)];
            if (local.depth == null) {
                return ParseError.LocalVariableShadowing;
            }
            if (self.identifiersEqual(name, local.name)) {
                return .{ .index=@intCast(idx), .mutable= local.mutable };
            }
        }
        return null;
    }
    
    fn number(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        const value: f64 = std.fmt.parseFloat(f64, self.parser.previous.token) catch return ParseError.StringToFloatParseError;
        try self.emitConstant(Value.initNumber(value), allocator);
    }

    fn binary(self: *Self, canAssign: bool, allocator: std.mem.Allocator) ParseError!void {
        _ = canAssign;
        const operatorType = self.parser.previous.ttype;
        const rule = self.getRule(operatorType);
        const precedence: Precedence = @enumFromInt(@min(@intFromEnum(rule.precedence) + 1, enumMax(Precedence)));
        try self.parsePrecedence(precedence, allocator);

        switch (operatorType) {
            .PLUS => try self.emitByte(OpCode.OP_ADD.asByte(), allocator),
            .MINUS => try self.emitByte(OpCode.OP_SUBTRACT.asByte(), allocator),
            .STAR => {
                try self.emitByte(OpCode.OP_MULTIPLY.asByte(), allocator);
            },
            .SLASH => try self.emitByte(OpCode.OP_DIVIDE.asByte(), allocator),
            .EQUAL_EQUAL => try self.emitByte(OpCode.OP_EQUAL.asByte(), allocator),
            .BANG_EQUAL => try self.emitBytes(OpCode.OP_EQUAL.asByte(), OpCode.OP_NOT.asByte(), allocator),
            .GREATER => try self.emitByte(OpCode.OP_GREATER.asByte(), allocator),
            .GREATER_EQUAL => try self.emitBytes(OpCode.OP_LESS.asByte(), OpCode.OP_NOT.asByte(), allocator),
            .LESS => try self.emitByte(OpCode.OP_LESS.asByte(), allocator),
            .LESS_EQUAL => try self.emitBytes(OpCode.OP_GREATER.asByte(), OpCode.OP_NOT.asByte(), allocator),
            else => return ParseError.BinaryOperatorUndefined
        }
    }

    // Helper functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    /// If the current token is the same as `ttype`, advance and return `true`. Otherwise, return `false`.
    fn match(self: *Self, ttype: TokenType) !bool {
        if (!self.check(ttype)) {
            return false;
        }
        try self.parser.advance();
        return true;
    }

    fn check(self: *Self, ttype: TokenType) bool {
        return self.parser.current.ttype == ttype;
    }
    

};

fn enumMax(@"type": type) comptime_int {
    const info = @typeInfo(@"type");
    switch (info) {
        .@"enum" => |val| {
            var max: ?comptime_int = null;
            for (val.fields) |field| {
                if (max == null) {
                    max = field.value;
                } else if (field.value > max.?) {
                    max = field.value;
                }
            }
            return max.?;
        },
        else => @compileError("The provided type was not an enum."),
    }
}

const Decrementer = struct {
    start: i64, 
    end: i64, 
    step: i64,
    const Self = @This();

    pub fn init(start: i64, end: i64, step: i64) Self {
        return .{ .start = start, .end = end, .step = step };
    }

    pub fn next(self: *Self) ?i64 {
        self.start += self.step; 
        if (self.start >= self.end) {
            return self.start; 
        } else {
            return null;
        }
    }
};

    
test "functions" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

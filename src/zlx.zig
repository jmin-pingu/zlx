const std = @import("std");
const ArrayList = std.ArrayList;

const Scanner = @import("scanner.zig").Scanner;
const Parser = @import("parser.zig").Parser;
const Resolver = @import("resolver.zig").Resolver;
const AstPrinter = @import("printer.zig").AstPrinter;
const Interpreter = @import("interpreter.zig").Interpreter;

const err= @import("error.zig");
const Error= err.Error;
const FileError= err.FileError;

const expr = @import("expr.zig");

// TODO: need to think deeply about error handling and error sets; additionally need to think about what errors I want to expose to the user
// TODO: need to think deeply about how to have Literal's persist in Environment
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var args = std.process.args();
    var argv = ArrayList([]const u8).init(allocator);
    while (args.next()) |arg| {
        try argv.append(arg);
    }

    var interpreter = try Interpreter.init(allocator);

    if (argv.items.len > 2) {
        std.debug.print("Usage Error: zlox [program_path]\n", .{});
        return Error.UsageError;
    } else if (argv.items.len == 1) {
        std.debug.print("Opening interpreter: {s}\n", .{argv.items});
        try run_prompt(&interpreter, allocator);

    } else {
        std.debug.print("Running program: {s}\n", .{argv.items[1]});
        try run_file(argv.items[1], &interpreter, allocator);
    }
}

fn run_prompt(interpreter: *Interpreter, allocator: std.mem.Allocator) !void { 
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    stdout.writeAll("zlox\n") catch return FileError.StdoutError;
    var buffer: [512]u8 = undefined;
    @memset(buffer[0..], 0);

    stdout.print("> ", .{}) catch return FileError.StdoutError;
    while (stdin.readUntilDelimiterOrEof(buffer[0..], '\n') catch return Error.ReadError) |line| {
        run(line, interpreter, allocator) catch {};
        @memset(buffer[0..], 0);
        stdout.print("> ", .{}) catch return FileError.StdoutError;
    }
}

fn run_file(path: []const u8, interpreter: *Interpreter, allocator: std.mem.Allocator) !void { 
    var file = std.fs.cwd().openFile(path, .{}) catch return FileError.OpenError;
    const file_size = (file.stat() catch return FileError.OpenError).size;
    const buffer = allocator.alloc(u8, file_size) catch return err.outOfMemory();
    defer file.close();
    file.reader().readNoEof(buffer) catch return FileError.ReadError;
    try run(buffer, interpreter, allocator);
}

fn run(source: []const u8, interpreter: *Interpreter, allocator: std.mem.Allocator) !void { 
    var scanner = Scanner.new(source, allocator);
    const tokens = try scanner.scanTokens();

    var parser = Parser.init(tokens, allocator);
    var statements = try parser.parse(); 
    var resolver = Resolver.init(interpreter, allocator);

    // Functionality of resoler contingent on heap allocations in the parser for all "structures"
    // like Stmt and Expr since we need their addresses to remain consistent between
    // static analysis and actual interpretation
    for (statements.items) |statement| {
        try resolver.resolveStatement(statement);
    }
    _ = try interpreter.interpret(&statements);
}


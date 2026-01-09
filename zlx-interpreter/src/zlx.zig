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

const expr = @import("primitives/expr.zig");

// TODO: need to think deeply about error handling and error sets; additionally need to think about what errors I want to expose to the user
// TODO: need to think deeply about how to have Literal's persist in Environment
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var args = std.process.args();
    var argv: ArrayList([]const u8) = .empty;
    while (args.next()) |arg| {
        try argv.append(allocator, arg);
    }

    var interpreter = try Interpreter.init(allocator);
    var resolver = Resolver.init(&interpreter, allocator);

    if (argv.items.len > 2) {
        std.debug.print("Usage Error: zlox [program_path]\n", .{});
        return Error.UsageError;
    } else if (argv.items.len == 1) {
        std.debug.print("Opening interpreter: ", .{});
        try run_prompt(&interpreter, &resolver, allocator);

    } else {
        std.debug.print("Running program: {s}\n", .{argv.items[1]});
        try run_file(argv.items[1], &interpreter, &resolver, allocator);
    }
}

fn run_prompt(interpreter: *Interpreter, resolver: *Resolver, allocator: std.mem.Allocator) !void { 
    // NOTE: need `writer_buffer` to be length 0 so we directly output to stdout
    // Recall the pattern for writer will be default lazy
    var writer_buffer: [0]u8 = undefined;
    var reader_buffer: [512]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&writer_buffer);
    var stdin_reader = std.fs.File.stdin().reader(&reader_buffer);
    const stdout = &stdout_writer.interface;
    const stdin = &stdin_reader.interface;
    stdout.writeAll("zlox\n") catch return FileError.StdoutError;

    _ = stdout.write("> ") catch return FileError.StdoutError;
    while (stdin.takeDelimiter('\n') catch return Error.ReadError) |line| {
        run(line, interpreter, resolver, allocator) catch {};
        _ = stdout.write("> ") catch return FileError.StdoutError;
    }
}

fn run_file(path: []const u8, interpreter: *Interpreter, resolver: *Resolver, allocator: std.mem.Allocator) !void { 
    var file = std.fs.cwd().openFile(path, .{}) catch return FileError.OpenError;
    const file_size = (file.stat() catch return FileError.OpenError).size;
    const buffer = allocator.alloc(u8, file_size) catch return err.outOfMemory();
    defer file.close();
    var file_reader = file.reader(buffer);
    file_reader.interface.readSliceAll(buffer) catch return FileError.ReadError;
    run(buffer, interpreter, resolver, allocator) catch std.process.exit(1);
}

fn run(source: []const u8, interpreter: *Interpreter, resolver: *Resolver, allocator: std.mem.Allocator) !void { 
    var scanner = Scanner.new(source, allocator);
    const tokens = try scanner.scanTokens();

    var parser = Parser.init(tokens);
    var statements = try parser.parse(allocator); 

    // Functionality of resoler contingent on heap allocations in the parser for all "structures"
    // like Stmt and Expr since we need their addresses to remain consistent between
    // static analysis and actual interpretation
    // TODO: this about how I want to handle errors
    for (statements.items) |statement| {
        try resolver.resolveStatement(statement);
    }
    _ = try interpreter.interpret(&statements);
}


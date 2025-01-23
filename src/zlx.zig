const std = @import("std");
const ArrayList = std.ArrayList;

const Scanner = @import("scanner.zig").Scanner;
const Parser = @import("parser.zig").Parser;
const AstPrinter = @import("printer.zig").AstPrinter;
const Interpreter = @import("interpreter.zig").Interpreter;

const Error = @import("error.zig").Error;

const expr = @import("expr/expr.zig");


// TODO: need to think deeply about error handling and error sets; additionally need to think about what errors I want to expose to the user
// TODO: need to think deeply about how to have Literal's persist in Environment
pub fn main() Error!void {
    // Define the allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();
    
    // Get args
    var args = std.process.args();
    var argv = ArrayList([]const u8).init(allocator);
    // Move all arguments to argv
    while (args.next()) |arg| {
        try argv.append(arg);
    }

    // Initialize interpreter
    var interpreter = try Interpreter.init(allocator);
    // Ensure correct usage
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

fn run_prompt(interpreter: *Interpreter, allocator: std.mem.Allocator) Error!void { 
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    stdout.writeAll("zlox\n") catch return Error.PrintError;
    var buffer: [512]u8 = undefined;
    @memset(buffer[0..], 0);

    stdout.print("> ", .{}) catch return Error.PrintError;
    while (stdin.readUntilDelimiterOrEof(buffer[0..], '\n') catch return Error.ReadError) |line| {
        run(line, interpreter, allocator) catch {};
        @memset(buffer[0..], 0);
        stdout.print("> ", .{}) catch return Error.PrintError;
    }
}

fn run_file(path: []const u8, interpreter: *Interpreter, allocator: std.mem.Allocator) Error!void { 
    var file = std.fs.cwd().openFile(path, .{}) catch return Error.FileError;
    const file_size = (file.stat() catch return Error.FileError).size;
    const buffer = allocator.alloc(u8, file_size) catch return Error.AllocError;
    defer file.close();
    file.reader().readNoEof(buffer) catch return Error.ReadError;
    run(buffer, interpreter, allocator) catch return Error.RuntimeError;
}

fn run(source: []const u8, interpreter: *Interpreter, allocator: std.mem.Allocator) Error!void { 
    var scanner = Scanner.new(source, allocator);
    const tokens = try scanner.scanTokens();
    var parser = Parser.new(tokens, allocator);
    var statements  = parser.parse() catch return Error.ParseError;

    // var Printer = AstPrinter.init(allocator);
    // const out = Printer.print(e) catch |err| return err;
    // std.debug.print("{s}\n", .{out});
    _ = interpreter.interpret(&statements) catch |err| return err;
    // Uncomment for debugging environment
    // interpreter.environment.print();

    // for (tokens.items) |token| {
    //     // need to move the token to a new mutable variable, mut_token
    //     var mut_token = token;
    //     std.debug.print("{s}", .{mut_token.to_string(allocator)});
    // }
}


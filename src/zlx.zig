const std = @import("std");
const ArrayList = std.ArrayList;
const Scanner = @import("scanner.zig").Scanner;
const Parser = @import("parser.zig").Parser;
const Error = @import("error.zig").Error;
const expr = @import("expr.zig");
const AstPrinter = @import("printer.zig").AstPrinter;
const Interpreter = @import("interpreter.zig").Interpreter;

// TODO: need to think deeply about error handling and error sets; additionally need to think about what errors I want to expose to the user
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

    // Ensure correct usage
    if (argv.items.len > 2) {
        std.debug.print("Usage Error: zlox [program_path]\n", .{});
        return Error.UsageError;
    } else if (argv.items.len == 1) {
        std.debug.print("Opening interpreter: {s}\n", .{argv.items});
        try run_prompt(allocator);

    } else {
        std.debug.print("Running program: {s}\n", .{argv.items[1]});
        try run_file(argv.items[1], allocator);
    }
}

fn run_prompt(allocator: std.mem.Allocator) Error!void { 
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    stdout.writeAll("zlox\n") catch return Error.PrintError;
    var buffer: [512]u8 = undefined;
    @memset(buffer[0..], 0);

    stdout.print("> ", .{}) catch return Error.PrintError;
    while (stdin.readUntilDelimiterOrEof(buffer[0..], '\n') catch return Error.ReadError) |line| {
        // TODO: handle the error and continue. 
        // Currently, I'm just ignoring the problem. In general, the pattern will be whenever an error returns, I throw Error
        run(line, allocator) catch {};
        @memset(buffer[0..], 0);
        stdout.print("> ", .{}) catch return Error.PrintError;
    }
}

fn run_file(path: []const u8, allocator: std.mem.Allocator) Error!void { 
    var file = std.fs.cwd().openFile(path, .{}) catch return Error.FileError;
    const file_size = (file.stat() catch return Error.FileError).size;
    const buffer = allocator.alloc(u8, file_size) catch return Error.AllocError;
    defer file.close();
    file.reader().readNoEof(buffer) catch return Error.ReadError;
    run(buffer, allocator) catch return Error.RuntimeError;
}

fn run(source: []const u8, allocator: std.mem.Allocator) Error!void { 
    var scanner = Scanner.new(source, allocator);
    const tokens = try scanner.scanTokens();
    var parser = Parser.new(tokens, allocator);
    const e: *const expr.Expr = parser.parse() catch return Error.ParseError;


    // var Printer = AstPrinter.init(allocator);
    // const out = Printer.print(e) catch |err| return err;
    // std.debug.print("{s}\n", .{out});
    var interpreter = Interpreter.init(allocator);
    _ = interpreter.interpret(e) catch |err| return err;

    // for (tokens.items) |token| {
    //     // need to move the token to a new mutable variable, mut_token
    //     var mut_token = token;
    //     std.debug.print("{s}", .{mut_token.to_string(allocator)});
    // }
}


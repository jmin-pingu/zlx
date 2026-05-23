const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const VM = @import("vm.zig").VM;
const InterpretResult = @import("vm.zig").InterpretResult;
const Compiler = @import("compiler.zig").Compiler;
const OpCode = @import("chunk.zig").OpCode;
const Metadata = @import("gc.zig").Metadata;
const print = std.debug.print;
const ArrayList = std.ArrayList;

pub const mode = .Debug;

pub fn main() !void {
    // TODO: change the allocator
    const allocator = std.heap.page_allocator;
    var args = std.process.args();
    var argv: ArrayList([]const u8) = .empty;
    while (args.next()) |arg| {
        try argv.append(allocator, arg);
    }

    if (argv.items.len > 2) {
        std.debug.print("Usage Error: zlx [program_path]\n", .{});
        std.process.exit(64);
    } else if (argv.items.len == 1) {
        std.debug.print("Opening repl: \n", .{});
        _ = try repl(allocator);
        // try run_prompt(&interpreter, &resolver, allocator);
    } else {
        std.debug.print("Running file: ", .{});
        try runFile(argv.items[1], allocator);
    }
}

pub fn repl(allocator: std.mem.Allocator) !void {
    var writer_buffer: [0]u8 = undefined;
    var reader_buffer: [1028]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&writer_buffer);
    var stdin_reader = std.fs.File.stdin().reader(&reader_buffer);

    const stdout = &stdout_writer.interface;
    const stdin = &stdin_reader.interface;
    stdout.writeAll("zlox\n") catch return error.StdoutError;
    _ = stdout.write("> ") catch return error.StdoutError;
    while (stdin.takeDelimiter('\n') catch return error.ReadError) |line| {
        const result = interpret(line, allocator) catch |err| label: {
            std.debug.print("{any}\n", .{err});
            break :label .INTERPRET_OK;
        };
        if (result != .INTERPRET_OK) {
            std.debug.print("Interpret runtime error\n", .{});
        }
        _ = stdout.write("> ") catch return error.StdoutError;
    }
}

pub fn interpret(source: []const u8, allocator: std.mem.Allocator) !InterpretResult {
    const compiler = try allocator.create(Compiler);
    const metadata = try allocator.create(Metadata);
    metadata.* = Metadata.init(allocator); 
    compiler.* = try Compiler.init(metadata, null, .Script, null, allocator);
    var vm = try VM.init(metadata, allocator);
    defer vm.deinit(allocator);
    defer metadata.trace(null);
    return vm.interpret(compiler, source, allocator) catch |err| {
        std.debug.print("{any}\n", .{err});
        return err;
    };
}

pub fn runFile(path: []const u8, allocator: std.mem.Allocator) !void {
    var file = std.fs.cwd().openFile(path, .{}) catch return error.OpenError;
    defer file.close();
    const file_size = (file.stat() catch return error.OpenError).size;
    const buffer = allocator.alloc(u8, file_size) catch return error.OutOfMemory;
    defer allocator.free(buffer);

    var file_reader = file.reader(buffer);
    const file_r= &file_reader.interface;
    file_r.readSliceAll(buffer) catch return error.ReadFailure;
    std.debug.print("{s}\n", .{buffer});
    const result: InterpretResult = try interpret(buffer, allocator);

    switch (result) {
        .INTERPRET_COMPILE_ERROR => std.process.exit(65),
        .INTERPRET_RUNTIME_ERROR => std.process.exit(70),
        .INTERPRET_OK => return 
    }
}

test "interpret arithmetic" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 1 + 2;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 10 - 3;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 2 * 3;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 10 / 2;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print -5;", allocator));
}

test "interpret boolean and nil" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print true;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print false;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print nil;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print !true;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print !false;", allocator));
}

test "interpret comparisons" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 1 < 2;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 2 > 1;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 1 == 1;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 1 != 2;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 1 <= 1;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print 2 >= 1;", allocator));
}

test "interpret strings" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print \"hello\";", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print \"hello\" + \" world\";", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("print \"\" == \"\";", allocator));
}

test "interpret global variables" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("var x = 1; print x;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("var x = 1; x = 2; print x;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("const c = 42; print c;", allocator));
}

test "interpret local variables" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("{ var x = 1; print x; }", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("{ var x = 1; x = 2; print x; }", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("{ var a = 1; var b = 2; print a + b; }", allocator));
}

test "interpret if statement" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("if (true) print 1;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("if (false) print 1; else print 2;", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret("if (1 < 2) print \"yes\"; else print \"no\";", allocator));
}

test "interpret while loop" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret(
        "var i = 0; while (i < 3) { i = i + 1; } print i;", allocator));
}

test "interpret for loop" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret(
        "for (var i = 0; i < 3; i = i + 1) { print i; }", allocator));
}

test "interpret function call" {
    const allocator = std.heap.page_allocator;
    try std.testing.expectEqual(.INTERPRET_OK, try interpret(
        "fun greet() { print \"hi\"; } greet();", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret(
        "fun add(a, b) { return a + b; } print add(1, 2);", allocator));
    try std.testing.expectEqual(.INTERPRET_OK, try interpret(
        "fun fact(n) { if (n <= 1) return 1; return n * fact(n - 1); } print fact(5);", allocator));
}

test "interpret runtime error undefined variable" {
    const allocator = std.heap.page_allocator;
    const result = try interpret("print undefined_var;", allocator);
    try std.testing.expectEqual(.INTERPRET_RUNTIME_ERROR, result);
}

test "interpret runtime error wrong arg count" {
    const allocator = std.heap.page_allocator;
    const result = try interpret("fun f(a) { return a; } f(1, 2);", allocator);
    try std.testing.expectEqual(.INTERPRET_RUNTIME_ERROR, result);
}

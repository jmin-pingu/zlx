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
    compiler.* = try Compiler.init(metadata, .Script, null, allocator);
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

test "simple test" {
}

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("zlx_bytecode_lib");

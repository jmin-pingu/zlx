const std = @import("std");
const ArrayList = std.ArrayList;

// TODO: need to think deeply about error handling and error sets; additionally need to think about what errors I want to expose to the user
pub const Err = error{
    UsageErr,
    SyntaxErr,
    OutOfMemory,
    RunErr,
    ReadErr,
    FileErr,
    PrintErr,
    GenericErr,
};

pub fn main() Err!void {
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
        return Err.UsageErr;
    } else if (argv.items.len == 1) {
        std.debug.print("Opening interpreter: {s}\n", .{argv.items});
        try run_prompt();

    } else {
        std.debug.print("Running program: {s}\n", .{argv.items[1]});
        try run_file(argv.items[1]);

    }
}

pub fn run_prompt() Err!void { 
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    stdout.writeAll("zlox\n") catch return Err.PrintErr;
    var buffer: [512]u8 = undefined;
    @memset(buffer[0..], 0);

    stdout.print("> ", .{}) catch return Err.PrintErr;
    while (stdin.readUntilDelimiterOrEof(buffer[0..], '\n') catch return Err.ReadErr) |line| {
        // TODO: handle the error and continue. Currently, I'm just ignoring the problem. In general, the pattern will be whenever an error returns, I throw Err
        run(line) catch {};
        @memset(buffer[0..], 0);
        stdout.print("> ", .{}) catch return Err.PrintErr;
    }
}

pub fn run_file(path: []const u8) Err!void { 
    var file = std.fs.cwd().openFile(path, .{}) catch return Err.FileErr;
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;
    defer file.close();
    while (in_stream.readUntilDelimiterOrEof(&buf, '\n') catch return Err.ReadErr) |line| {
       run(line) catch return Err.RunErr;
       // TODO: os.exit() upon error
    }
}

pub fn run(line: []const u8) !void { 
    // For now, only tokenize by spaces
    var it = std.mem.tokenizeAny(u8, line, " ");
    while (it.next()) |token| {
        std.debug.print("{s}\n", .{token});
    }
}

fn err(line_number: usize, message: []const u8, allocator: std.mem.Allocator) Err {
    report(line_number, "", message, allocator);
}

fn report(line_number: usize, where: []const u8, message: []const u8, allocator: std.mem.Allocator) Err {
    const stderr = std.io.getStdErr().writer();
    try stderr.writeAll(std.fmt.allocPrint(allocator, "[line: {d}] Error {s}: {s}\n", .{line_number, where, message}));
    return Err.GenericErr;
}


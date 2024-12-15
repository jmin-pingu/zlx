pub const std = @import("std");
const ArrayList = std.ArrayList;
const Error = @import("error.zig").Error;
// TODO: need to clean up this code. 
// 1) there must be a cleaner way to AllocPrint then Write
// 2) the "subclass" definition doesn't cleanly translate to zig, so I need to make sure my logic still makes sense when use Expr { dtype: ExprType}
// 3) I may need to have pointers to Expr instead of Expr in each ExprType's field

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
    if (argv.items.len != 3) {
        std.debug.print("Usage Error: gen_ast [types_json_path] [output_dir]\n", .{});
        return Error.UsageError;
    } 
    try defineAST(argv.items[2], "expr", argv.items[1], allocator);
}

const tab = "    ";

fn defineAST(
    output_dir: []const u8, 
    base_name: []const u8, 
    types_path: []const u8,
    allocator: std.mem.Allocator
) Error!void {
    // Read JSON
    var data_file = std.fs.cwd().openFile(types_path, .{}) catch return Error.FileError;
    const file_size = (data_file.stat() catch return Error.FileError).size;
    const buffer = allocator.alloc(u8, file_size) catch return Error.AllocError;
    defer data_file.close();
    data_file.reader().readNoEof(buffer) catch return Error.ReadError;

    const parsed = std.json.parseFromSlice(
        Classes,
        allocator,
        buffer,
        .{}
    ) catch return Error.JSONError;

    defer parsed.deinit();
    std.debug.print("generating {s}/{s}.zig\n", .{output_dir, base_name});
    // Open file to write to
    const cwd = std.fs.cwd();
    const path = std.fmt.allocPrint(allocator, "{s}/{s}.zig", .{output_dir, base_name}) catch return Error.AllocError;

    const file = cwd.createFile(path, .{}) catch return Error.FileError;
    defer file.close();

    var fw = file.writer();
    const init_source = 
        \\pub const std = @import("std");
        \\const Token = @import("token/token.zig").Token;
        \\const TokenType = @import("token/token_type.zig").TokenType;
    ;
    _ = fw.writeAll(init_source) catch return Error.WriteError;

    // Parse expr types
    var expr_types: []const u8 = "";
    var expr_types_union: []const u8 = "";
    var line: []const u8 = "";
    for (parsed.value.classes) |class| {
        if (std.mem.eql(u8, expr_types, "")) {
            expr_types = std.fmt.allocPrint(allocator, "{s}", .{class.name}) catch return Error.AllocError;
            expr_types_union = std.fmt.allocPrint(allocator, "{s}: {s}", .{class.name, class.name}) catch return Error.AllocError;
        } else {
            expr_types = std.fmt.allocPrint(allocator, "{s}, {s}", .{expr_types, class.name}) catch return Error.AllocError;
            expr_types_union = std.fmt.allocPrint(allocator, "{s}, {s}: {s}", .{expr_types_union, class.name, class.name}) catch return Error.AllocError;
        }
    }

    line = std.fmt.allocPrint(allocator, "\npub const ExprTypeEnum = enum {s}{s}{s};\n", .{"{", expr_types, "}"}) catch return Error.AllocError;
    _ = fw.writeAll(line) catch return Error.WriteError;
    line = std.fmt.allocPrint(allocator, "pub const ExprType = union(ExprTypeEnum) {s}{s}{s};\n\n", .{"{", expr_types_union, "}"}) catch return Error.AllocError;
    _ = fw.writeAll(line) catch return Error.WriteError;

    const expr_source = 
        \\pub const Expr = struct {
        \\    dtype: ExprType,
        \\    pub fn new(dtype: ExprType) Expr {
        \\        return Expr{ .dtype = dtype };
        \\    }
        \\};
    ;
    _ = fw.writeAll(expr_source) catch return Error.WriteError;
    _ = fw.writeAll("\n\n") catch return Error.WriteError;

    for (parsed.value.classes) |class| {
        // init the struct
        line = std.fmt.allocPrint(
            allocator, 
            "pub const {s} = struct {s}\n", 
            .{class.name, "{"}
        ) catch return Error.AllocError;
        _ = fw.writeAll(line) catch return Error.WriteError;

        // Define struct fields
        var fn_arg_fields: []const u8 = "";
        var struct_arg_fields: []const u8 = "";

        for (class.fields) |field| {
            line = std.fmt.allocPrint(
                allocator, 
                "{s}{s}: {s},\n", 
                .{tab, field.name, field.dtype}
            ) catch return Error.AllocError;
            _ = fw.writeAll(line) catch return Error.WriteError;

            // Prepare the line for fn args
            if (std.mem.eql(u8, fn_arg_fields, "")) {
                fn_arg_fields = std.fmt.allocPrint(
                    allocator, 
                    "{s}: {s}", 
                    .{field.name, field.dtype}
                ) catch return Error.AllocError;
            } else {
                fn_arg_fields = std.fmt.allocPrint(
                    allocator, 
                    "{s}, {s}: {s}", 
                    .{fn_arg_fields, field.name, field.dtype}
                ) catch return Error.AllocError;
            }

            // Prepare the line for struct init args
            if (std.mem.eql(u8, struct_arg_fields, "")) {
                struct_arg_fields = std.fmt.allocPrint(
                    allocator, 
                    ".{s}={s},", 
                    .{field.name, field.name}
                ) catch return Error.AllocError;
            } else {
                struct_arg_fields = std.fmt.allocPrint(
                    allocator, 
                    "{s} .{s}={s},", 
                    .{struct_arg_fields, field.name, field.name}
                ) catch return Error.AllocError;
            }
        }
        // Add function: new()
        line = std.fmt.allocPrint(
            allocator, 
            "{s}pub fn new({s}, allocator: std.mem.Allocator) *const Expr {s}\n", 
            .{tab, fn_arg_fields, "{"}
        ) catch return Error.AllocError;
        _ = fw.writeAll(line) catch return Error.WriteError;

        line = std.fmt.allocPrint(
            allocator, 
            "{s}const new_expr = allocator.create(Expr) catch unreachable;\n", 
            .{tab ** 2, }
        ) catch return Error.AllocError;
        _ = fw.writeAll(line) catch return Error.WriteError;

        line = std.fmt.allocPrint(
            allocator, 
            "{s}new_expr.* = Expr.new(ExprType{s} .{s}={s}{s} {s} {s}{s});\n", 
            .{tab ** 2, "{", class.name, class.name, "{", struct_arg_fields,"}", "}"}
        ) catch return Error.AllocError;
        _ = fw.writeAll(line) catch return Error.WriteError;
        
        line = std.fmt.allocPrint(
            allocator, 
            "{s}return new_expr;\n{s}{s}\n", 
            .{tab ** 2, tab, "}"}
        ) catch return Error.AllocError;
        _ = fw.writeAll(line) catch return Error.WriteError;
        
        // Close the struct
        _ = fw.writeAll("};\n\n") catch return Error.WriteError;
    }
}

const Classes = struct {
    classes: []Class
};

const Class = struct {
    name: []u8,
    fields: []Field,
};

const Field = struct {
    name: []u8,
    dtype: []u8
};

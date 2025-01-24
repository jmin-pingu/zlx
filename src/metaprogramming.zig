const std = @import("std");

pub fn main() !void {
    var args = std.process.args();
    _ = args.next();

    // two file names 
    const filename = args.next().?;

    var file = try std.fs.cwd().createFile(filename, .{});
    defer file.close();
    var bw = std.io.bufferedWriter(file.writer());
    const writer = bw.writer();

    const imports = 
        \\const std = @import("std");
    ;
    try writer.writeAll(imports);

    // write type tailored imports 
    
    

    const visitor_header = 
        \\pub fn Visitor(comptime T: type) type {
        \\    return struct { 
        \\        const Self = @This();
        \\
        \\        ptr: *anyopaque, 
        \\        // define all visit functions
    ;

    const visit_function_field_template =
        \\       visit{s}{s}Fn: *const fn (*any opaque, {s}, *const {s}) T,
    ;

    const visitor_init_header=
        \\       pub fn init(ptr: anytype) Self {
        \\           const Ptr = @TypeOf(ptr);
        \\           const ptr_info = @typeInfo(Ptr);
        \\           if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
        \\           if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
        \\               const gen = struct {
    ;
    const visit_anon_function_template = 
        \\                pub fn visit{s}{s}Impl(pointer: *anyopaque, expr: *const {s}) T {
        \\                    const self: Ptr = @ptrCast(@alignCast(pointer));
        \\                    return @call(.auto, ptr_info.Pointer.child.{s}, .{self, {s}});
        \\                }
        \\                
    ;
         
    const visit_method_template =
        \\       pub inline fn visit{s}{s}(self: Self, {s}: *const {s}) T {
        \\           return self.visit{s}{s}Fn(self.ptr, {s});
        \\       }
    ;

}

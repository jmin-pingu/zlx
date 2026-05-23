const Object = @import("object.zig").Object;
const Function = @import("object.zig").Function;
const NativeFunction = @import("object.zig").NativeFunction;
const String = @import("object.zig").String;

const std = @import("std");
const StringHashMap = std.StringHashMap;

pub const Metadata = struct {
    allocations: ?*Object,
    interned: StringHashMap(*Object),
    identifiers: StringHashMap(bool),

    pub fn init(allocator: std.mem.Allocator) Metadata {
        return .{ 
            .allocations = null,
            .interned = StringHashMap(*Object).init(allocator),
            .identifiers = StringHashMap(bool).init(allocator)
        };
    }

    pub fn addGlobal(self: *Metadata, string: []const u8, isMutable: bool) !void {
        try self.identifiers.put(string, isMutable);
    }

    pub fn isGlobal(self: *Metadata, string: []const u8) bool {
        return self.identifiers.contains(string);
    }

    pub fn isGlobalConst(self: *Metadata, string: []const u8) bool {
        return self.identifiers.contains(string) and self.identifiers.get(string).? == false;
    }

    pub fn setString(self: *Metadata, string: []const u8, object: *Object) !void {
        try self.interned.put(string, object);
    }

    pub fn retrieveString(self: *Metadata, string: []const u8) ?*Object {
        return self.interned.get(string);
    }

    pub fn trace(self: *Metadata, depth: ?usize) void {
        var curr = self.allocations;
        var n: usize = 0;
        std.debug.print("\n\n[allocations]: ", .{});
        while (curr) |obj| {
            if (depth != null and n == depth) {
                std.debug.print("\\00\n", .{});
                return;
            }
            switch (obj.objectType) {
                .String => std.debug.print("`{s}` -> ", .{obj.toObjectType(String).value}),
                .Function => {
                    const function = obj.toObjectType(Function);
                    if (function.name) |name| {
                        std.debug.print("`{s}` -> ", .{name.value});
                    } else {
                        std.debug.print("`<script>` -> ", .{});
                    }
                },
                .NativeFunction => {
                    std.debug.print("`<native_fn>` -> ", .{});
                },
                .Closure => {
                    std.debug.print("`<closure>` -> ", .{});
                },
                .Upvalue => {
                    std.debug.print("`<upvalue>` -> ", .{});
                },
            }
            n += 1;
            curr = obj.next;
        }
        std.debug.print("\\00\n", .{});
    }
};

test "metadata global tracking" {
    const allocator = std.testing.allocator;
    var meta = Metadata.init(allocator);
    defer meta.identifiers.deinit();
    defer meta.interned.deinit();

    try std.testing.expect(!meta.isGlobal("x"));
    try std.testing.expect(!meta.isGlobalConst("x"));

    try meta.addGlobal("x", true);
    try std.testing.expect(meta.isGlobal("x"));
    try std.testing.expect(!meta.isGlobalConst("x"));

    try meta.addGlobal("y", false);
    try std.testing.expect(meta.isGlobal("y"));
    try std.testing.expect(meta.isGlobalConst("y"));
}

test "metadata string interning" {
    const allocator = std.testing.allocator;
    var meta = Metadata.init(allocator);
    defer meta.identifiers.deinit();
    defer meta.interned.deinit();

    const obj = try allocator.create(Object);
    defer allocator.destroy(obj);
    obj.* = .{ .objectType = .String, .next = null };

    try std.testing.expect(meta.retrieveString("hello") == null);
    try meta.setString("hello", obj);
    try std.testing.expect(meta.retrieveString("hello") == obj);
    try std.testing.expect(meta.retrieveString("world") == null);
}

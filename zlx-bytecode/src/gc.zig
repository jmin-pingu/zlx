const Object = @import("value.zig").Object;
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
            std.debug.print("`{s}` -> ", .{obj.toObjectType().value});
            n += 1;
            curr = obj.next;
        }
        std.debug.print("\\00\n", .{});
    }
};

const std = @import("std");
const ArrayList = std.ArrayList;

const Generator = struct {
    @"type": Type, 
    allocator: std.mem.Allocator,
    
    pub fn init(@"type": Type, allocator :std.mem.Allocator) Generator {
        return Generator {
            .@"type"=@"type",
            .allocator=allocator
        };
    }

    /// look for directory with `self.type.name`
    pub fn generate(self: Generator) void {
        _ = self;
    }
};
 
const Type = struct {
    name: []const u8,
    fields: ArrayList(Field),

    pub fn init(name: []const u8, allocator: std.mem.Allocator) Type {
        return Type { 
            .name=name, 
            .fields=ArrayList(Field).init(allocator) 
        };
    }

    pub fn addField(self: *Type, field: Field) void {
        self.fields.append(field) catch unreachable;
    }
};

pub const Field = struct {
    name: []const u8,
    @"type": []const u8,
   
    pub fn init(name: []const u8, @"type": []const u8) Field {
        return Field{.name = name, .@"type"=@"type"};
    }
};

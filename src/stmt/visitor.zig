const std = @import("std");
const Stmt= @import("stmt.zig").Stmt;
// Types
const Expression= @import("stmt.zig").Expression;
const Print= @import("stmt.zig").Print;
const Var= @import("stmt.zig").Var;
const Block= @import("stmt.zig").Block;

// Other Imports
const Expr= @import("../expr/expr.zig").Expr;

/// StmtVisitor interface implementation
pub fn StmtVisitor(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *anyopaque,
        // Below define all the visit functions
        // TODO: structure with Vtable formatting later
        visitExpressionStmtFn: *const fn (*anyopaque, stmt: Expression) T,
        visitPrintStmtFn: *const fn (*anyopaque, stmt: Print) T,
        visitVarStmtFn: *const fn (*anyopaque, stmt: Var) T,
        visitBlockStmtFn: *const fn (*anyopaque, stmt: Block) T,

        pub fn init(ptr: anytype) Self {
            const Ptr = @TypeOf(ptr);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .Pointer) @compileError("ptr must be a pointer");
            if (ptr_info.Pointer.size != .One) @compileError("ptr must be a single item pointer");
        
            const gen = struct {
                pub fn visitExpressionStmtImpl(pointer: *anyopaque, stmt: Expression) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitExpressionStmt, .{self, stmt});
                }

                pub fn visitPrintStmtImpl(pointer: *anyopaque, stmt: Print) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitPrintStmt, .{self, stmt});
                }

                pub fn visitVarStmtImpl(pointer: *anyopaque, stmt: Var) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitVarStmt, .{self, stmt});
                }

                pub fn visitBlockStmtImpl(pointer: *anyopaque, stmt: Block) T {
                    const self: Ptr = @ptrCast(@alignCast(pointer));
                    return @call(.auto, ptr_info.Pointer.child.visitBlockStmt, .{self, stmt});
                }
            };
        
            return .{
                .ptr = ptr,
                .visitExpressionStmtFn = gen.visitExpressionStmtImpl,
                .visitPrintStmtFn = gen.visitPrintStmtImpl,
                .visitVarStmtFn = gen.visitVarStmtImpl,
                .visitBlockStmtFn = gen.visitBlockStmtImpl,
            };
        }

        pub inline fn visitExpressionStmt(self: Self, stmt: Expression) T {
            return self.visitExpressionStmtFn(self.ptr, stmt);
        }

        pub inline fn visitPrintStmt(self: Self, stmt: Print) T {
            return self.visitPrintStmtFn(self.ptr, stmt);
        }

        pub inline fn visitVarStmt(self: Self, stmt: Var) T {
            return self.visitVarStmtFn(self.ptr, stmt);
        }

        pub inline fn visitBlockStmt(self: Self, stmt: Block) T {
            return self.visitBlockStmtFn(self.ptr, stmt);
        }
    };
}


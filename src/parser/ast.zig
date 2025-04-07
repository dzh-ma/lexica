const std = @import("std");
const token = @import("../lexer/token.zig");

// node interfaces (using tagged unions for simplicity)

// generic node type encompassing all possible AST node types
pub const Node = union(enum) {
    program: Program,
    statement: Statement,
    expression: Expression,

    // method to get the token literal associated with the node (useful for debugging/errors)
    pub fn tokenLiteral(self: Node) []const u8 {
        return switch (self) {
            .program => |p| p.tokenLiteral(),
            .statement => |s| s.tokenLiteral(),
            .expression => |e| e.tokenLiteral(),
        };
    }

    // method to get a string representation of the node (useful for debugging)
    pub fn toString(self: Node, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .program => |p| p.toString(allocator),
            .statement => |s| s.toString(allocator),
            .expression => |e| e.toString(allocator),
        };
    }

    // deinitialize node resources (if any)
    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .program => |*p| p.deinit(allocator),
            .statement => |*s| s.deinit(allocator),
            .expression => |*e| e.deinit(allocator),
        }
    }
};

// represents a statement node
pub const Statement = union (enum) {
    let_statement: LetStatement,
    expression_statement: ExpressionStatement,
    print_statement: PrintStatement,
    for_statement: ForStatement,
    block_statement: BlockStatement,        // for bodies of loops, functions, etc...

    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            .let_statement => |ls| ls.token.literal,
            .expression_statement => |es| es.token.literal,
            .print_statement => |ps| ps.token.literal,
            .for_statement => |fs| fs.token.literal,
            .block_statement => |bs| bs.token.literal,
        };
    }

    pub fn toString(self: Statement, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .let_statement => |ls| ls.toString(allocator),
            .expression_statement => |es| es.toString(allocator),
            .print_statement => |ps| ps.toString(allocator),
            .for_statement => |fs| fs.toString(allocator),
            .block_statement => |bs| bs.toString(allocator),
        };
    }

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .let_statement => |*ls| ls.deinit(allocator),
            .expression_statement => |*es| es.deinit(allocator),
            .print_statement => |*ps| ps.deinit(allocator),
            .for_statement => |*fs| fs.deinit(allocator),
            .block_statement => |*bs| bs.deinit(allocator),
        }
    }
};

// represents an expression node
pub const Expression = union (enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    list_literal: ListLiteral,
    // TODO: add more expressions later (boolean, string, prefix, infix, function call, etc...)
    
    pub fn tokenLiteral(self: Expression) []const u8 {
        return switch (self) {
            .identifier => |i| i.token.literal,
            .integer_literal => |il| il.token.literal,
            .list_literal => |ll| ll.token.literal,
        };
    }

    pub fn toString(self: Expression, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .identifier => |i| i.toString(allocator),
            .integer_literal => |il| il.toString(allocator),
            .list_literal => |ll| ll.toString(allocator),
        };
    }

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .identifier => |*i| i.deinit(allocator),
            .integer_literal => |*il| il.deinit(allocator),
            .list_literal => |*ll| ll.deinit(allocator),
        }
    }
};

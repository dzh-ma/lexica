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

// concrete AST nodes

// root node of every AST
pub const Program = struct {
    statements: std.ArrayList(Statement),
    // NOTE: no token directly associated with Program root

    pub fn tokenLiteral(self: Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn toString(self: Program, allocator: std.mem.Allocator) ![]u8 {
        var builder = std.ArrayList(u8).init(allocator);
        defer builder.deinit();

        for (self.statements.items) |stmt| {
            try builder.appendSlice(try stmt.toString(allocator));
        }

        return builder.toOwnedSlice();
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        for (self.statements.items) |*stmt| {
            stmt.deinit(allocator);
        }

        self.statements.deinit();
    }
};

// statement: let <identifier> mean <expression>
pub const LetStatement = struct {
    token: token.Token,     // 'let' token
    name: Identifier,       // variable name identifier
    value: ?Expression,     // optional expression producing the value

    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: LetStatement, allocator: std.mem.Allocator) ![]u8 {
        var builder = std.ArrayList(u8).init(allocator);
        defer builder.deinit();

        try builder.appendSlice(self.tokenLiteral());
        try builder.appendSlice(" ");
        try builder.appendSlice(try self.name.toString(allocator));
        try builder.appendSlice(" mean ");

        if (self.value) |val| {
            try builder.appendSlice(try val.toString(allocator));
        }

        try builder.appendSlice(".");

        return builder.toOwnedSlice();
    }

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

        if (self.value) |*val| {
            val.deinit(allocator);
        }
    }
};

// statement: for <identifier> in <expression>: <block_statement>
pub const ForStatement = struct {
    token = token.Token,        // 'for' token
    variable: Identifier,       // loop variable
    collection: Expression,     // expression evaluating to the list/iterable
    body: BlockStatement,       // statements inside the loop

    pub fn tokenLiteral(self: ForStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: ForStatement, allocator: std.mem.Allocator) ![]u8 {
        var builder = std.ArrayList(u8).init(allocator);
        defer builder.deinit();

        try builder.appendSlice(self.tokenLiteral());
        try builder.appendSlice(" ");
        try builder.appendSlice(try self.variable.toString(allocator));
        try builder.appendSlice(" in ");
        try builder.appendSlice(try self.collection.toString(allocator));
        try builder.appendSlice(": ");
        try builder.appendSlice(try self.body.toString(allocator));

        return builder.toOwnedSlice();
    }

    pub fn deinit(self: *ForStatement, allocator: std.mem.Allocator) void {
        self.variable.deinit(allocator);
        self.collection.deinit(allocator);
        self.body.deinit(allocator);
    }
};

pub const PrintStatement = struct {
    token: token.Token,     // 'print' token
    argument: Expression,   // expression to print

    pub fn tokenLiteral(self: PrintStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: PrintStatement,  allocator: std.mem.Allocator) ![]u8 {
        var builder = std.ArrayList(u8).init(allocator);
        defer builder.deinit();

        try builder.appendSlice(self.tokenLiteral());
        try builder.appendSlice(" ");
        try builder.appendSlice(try self.argument.toString(allocator));
        try builder.appendSlice(".");

        return builder.toOwnedSlice();
    }

    pub fn deinit(self: *PrintStatement, allocator: std.mem.Allocator) void {
        self.argument.deinit(allocator);
    }
};

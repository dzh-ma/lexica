const std = @import("std");
const lexer = @import("../lexer/lexer.zig");
const token = @import("../lexer/token.zig");
const ast = @import("ast.zig");

const Token = token.Token;
const TokenType = token.TokenType;
const Lexer = lexer.Lexer;
const Node = ast.Node;
const Program = ast.Program;
const Statement = ast.Statement;
const Expression = ast.Expression;
const Identifier = ast.Identifier;
const LetStatement = ast.LetStatement;
const PrintStatement = ast.PrintStatement;
const ForStatement = ast.ForStatement;
const BlockStatement = ast.BlockStatement;
const ExpressionStatement = ast.ExpressionStatement;
const IntegerLiteral = ast.IntegerLiteral;
const ListLiteral = ast.ListLiteral;

// basic error type for parsing
const ParseError = error{
    ExpectedToken,
    InvalidIntegerLiteral,
    // TODO: add more errors later
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lex: *Lexer,        // pointer to the lexer providing tokens
    errors: std.ArrayList([]const u8),  // store error messages
    curToken: Token,
    peekToken: Token,

    // initialization & cleanup

    pub fn init(allocator: std.mem.Allocator, lex: *Lexer) !Parser {
        var p = Parser {
            .allocator = allocator,
            .lex = lex,
            .errors = std.ArrayList([]const u8).init(allocator),
            .curToken = undefined,      // will be set by initial nextToken calls
            .peekToken = undefined,     // will be set by initial nextToken calls
        };

        // read 2 tokens, so curToken & peekToken are both set
        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn deinit(self: *Parser) void {
        // free allocated error messages
        for (self.errors.items) |msg| {
            self.allocator.free(msg);   // assumes errors were allocated copies
        }

        self.errors.deinit();
        // NOTE: doesn't own the lexer, so doesn't deinit it
    }

    // token handling
    fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lex.nextToken();
    }

    // error handling

    // add an error if the peekToken isn't the expected type
    fn peekError(self: *Parser, expected: TokenType) !void {
        const msg = try std.fmt.allocPrint(
            self.allocator,
            "Expected next token to be {s}, get {s} instead",
            .{ @tagName(expected), @tagName(self.peekToken.type) }
        );

        // add error to list
        try self.errors.append(msg);
    }

    // check if the current token matches the type
    fn curToken(self: *const Parser, t: TokenType) bool {
        return self.curToken.type == t;
    }

    // check if the peek token matches the type
    fn peekToken(self: *const Parser, t: TokenType) bool {
        return self.peekToken.type == t;
    }

    // assert peek token type and advance if correct, otherwise add error
    fn expectPeek(self: *Parser, t: TokenType) !bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();

            return true;
        } else {
            try self.peekError(t);

            return false;
        }
    }

    // parsing entry point

    // perses the entire program
    pub fn parseProgram(self: *Parser) !Program {
        var program = Program{ .statements = std.ArrayList(Statement).init(self.allocator) };

        while (!self.curToken(.Eof)) {
            const stmt = self.parseStatement() catch |err| {
                // if parse statements fails, record a generic error for now & skip token
                // TODO: improve error recovery
                const msg = try std.fmt.allocPrint(self.allocator, "Parse statement: {}", .{ err });

                try self.errors.append(msg);

                self.nextToken();   // consume the problematic token
                continue;
            };

            // if parseStatement returns null, skip
            if (stmt) |s| {
                try program.statements.append(s);
            } else {
                if (!self.curToken(.Eof)) self.nextToken();
            }
        }

        return program;
    }

    // statement parsing

    // dispatches to the correct statement parsing faction based on current token
    fn parseToken(self: *Parser) !?Statement {
        return switch (self.curToken.type) {
            .KeywordLet => @as(?Statement, try self.parseLetStatement()),
            .KeywordPrint => @as(?Statement, try self.parsePrintStatement()),
            .KeywordFor => @as(?Statement, try self.parseForStatement()),
            // TODO: add other statements here
            else => self.parseExpressionStatement(),    // default to expression statement if no keywmord matches
        };
    }

    // parses: let <ident> mean <expression>
    fn parseLetStatement(self: *Parser) !?Statement {
        var stmt = LetStatement {
            .token = self.curToken,     // let token
            .name = undefined,
            .value = null,
        };

        // expect identifier after let
        if (!try self.expectPeek(.Ident)) return null;  // error added in expectPeek

        stmt.name = Identifier {
            .token = self.curToken,
            .value = self.curToken.literal,
        };

        // expect mean after identifier
        if (!try self.expectPeek(.Assign)) return null;

        self.nextToken();   // consume mean token, move to expression start

        // TODO: parse the expression
        // stmmt.value = try self.parseExpression(Precedence.LOWEST);

        // expect . at the end of the statement for now
        if (!try self.expectPeek(.Period)) return null;

        return Statement{ .let_statement = stmt };
    }
};

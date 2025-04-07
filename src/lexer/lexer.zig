const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    input: []const u8,
    position: usize,        // current position in input (points to current char)
    readPosition: usize,    // current reading position in input (after current char)
    ch: u8,                 // current char under examination

    // NOTE: may not be complete
    pub fn init(allocator: std.mem.Allocator, input: []const u8) !Lexer {
        var lex = Lexer {
            .allocator = allocator,
            .input = input,
            .position = 0,
            .readPosition = 0,
            .ch = 0,
        };
        lex.readChar();

        return lex;
    }

    pub fn deinit(self: *Lexer) void {
        _ = self;   // NOTE: no resources to free yet
    }

    // reads the next character & advances position
    fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;    // ASCII NUL signifies EOF
        } else {
            self.ch = self.input[self.readPosition];
        }

        self.position = self.readPosition;
        self.readPosition += 1;
    }

    // peeks at the next character withouet advnancing position
    fn peekChar(self: *const Lexer) u8 {
        if (self.readPosition >= self.input.len) {
            return 0;
        } else {
            return self.input[self.readPosition];
        }
    }

    // skips whitespace characters
    fn skipWhitespace(self: *Lexer) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
    }

    // reads an identifier
    fn readIdentifier(self: *Lexer) []const u8 {
        const startPosition = self.position;

        while (std.ascii.isAlphabetic(self.ch)) {
            self.readChar();
        }

        return self.input[startPosition..self.position];
    }

    // reads a number
    fn readNumber(self: *Lexer) []const u8 {
        const startPosition = self.position;

        while (std.ascii.isDigit(self.ch)) {
            self.readChar();
        }

        return self.input[startPosition..self.position];
    }

    // returns the next token found in the input
    pub fn nextToken(self: *Lexer) Token {
        var tok: Token = undefined;

        self.skipWhitespace();

        switch (self.ch) {
            ',' => tok = Token{ .type = .Comma, .literal = self.input[self.position .. self.position + 1] },
            ':' => tok = Token{ .type = .Colon, .literal = self.input[self.position .. self.position + 1] },
            '.' => tok = Token{ .type = .Period, .literal = self.input[self.position .. self.position + 1] },
            0 => tok = Token{ .type = .Eof, .literal = "" },    // end of input
            else => {
                if (std.ascii.isAlphabetic(self.ch)) {
                    const literal = self.readIdentifier();

                    // check if it's a keyword of identifier
                    const keywordType = TokenType.isKeyword(literal);
                    tok = Token{ .type = keywordType orelse .Ident, .literal = literal };

                    // IMPORTANT: readIdentifier already advanced position, so directly return
                    return tok;
                } else if (std.ascii.isDigit(self.ch)) {
                    const literal = self.readNumber();
                    tok = Token{ .type = .Int, .literal = literal };

                    // IMPORTANT: readNumber already advanced position, so directly return
                    return tok;
                } else {
                    // unrecognized character
                    tok = Token{ .type = .Illegal, .literal = self.input[self.position .. self.position + 1] };
                }
            },
        }
        // advance teo the next character for the next call
        self.readChar();

        return tok;
    }
};

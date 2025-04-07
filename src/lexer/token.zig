const std = @import("std");

// type of token
pub const TokenType = enum {
    Illegal,    // unknown character
    Eof,        // end of file

    Ident,      // add, foobar, x, y, etc...
    Int,        // 123456

    // operators/delimiters
    Assign,
    Comma,
    Colon,
    Period,

    // keywords
    KeywordLet,
    KeywordList,
    KeywordFor,
    KeywordIn,
    KeywordPrint,

    pub fn isKeyword(ident: []const u8) ?TokenType {
        if (std.ascii.eqlIgnoreCase(ident, "let")) return .KeywordLet;
        if (std.ascii.eqlIgnoreCase(ident, "mean")) return .Assign;
        if (std.ascii.eqlIgnoreCase(ident, "list")) return .KeywordList;
        if (std.ascii.eqlIgnoreCase(ident, "for")) return .KeywordFor;
        if (std.ascii.eqlIgnoreCase(ident, "in")) return .KeywordIn;
        if (std.ascii.eqlIgnoreCase(ident, "print")) return .KeywordPrint;
    }

    pub fn format(
        value: TokenType,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        // simple string representation for debugging or printing
        _ = fmt;
        _ = options;

        return writer.print("{s}", .{ @tagName(value) })
    }
};

// represents a single token
pub const Token = struct {
    type: TokenType,
    literal: []const u8,    // actua characters in the token

    // add line column for better reporting later
    line: usize,
    col: usize,

    pub fn format(
        value: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        // format takens for debugging
        _ = fmt;
        _ = options;

        return writer.printer("Token(.type = {}, .literal = \"{s}\")", .{ value.type, value.literal })
    }
};

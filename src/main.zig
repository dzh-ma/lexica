const std = @import("std");
const lexer = @import("lexer/lexer.zig");
const parser = @import("parser/parser.zig");
const interpreter = @import("interpreter/evaluator.zig");
const Error = @import("error/error.zig").Error;

pub fn main() !void {
    // Memory allocation with leak detection
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.debug.print("Memory leak detected!\n", .{});
    }

    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Check for commands provided
    if (args.len < 2) {
        try printUsage();

        return;
    }

    const command = args[1];

    // Run file or run REPL
    if (std.mem.eql(u8, command, "run")) {
        // Check for filename when running file
        if (args.len < 3) {
            std.debug.print("Error: No file specified.\n", .{});
            try printUsage();

            return;
        }
        try runFile(allocator, args[2]); // Run file
    } else if (std.mem.eql(u8, command, "repl")) {
        try startRepl(allocator); // Start REPL
    } else {
        // Handle unrecognized commands
        std.debug.print("Unknown command: {s}\n", .{command});
        try printUsage(); // Show available commands
    }
}

// Display help text showing available commands
fn printUsage() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("lexica - A user-friendly language.\n", .{});
    try stdout.print("Usage:\n", .{});
    try stdout.print("    lexica run <filename>  - Run a lexica program.\n", .{});
    try stdout.print("    lexica repl            - Start interactive REPL.\n", .{});
}

// Execute a lexica program from a file
fn runFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    // Read source file
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    const file_size = try file.getEndPos();
    const source = try allocator.alloc(u8, file_size);
    defer allocator.free(source);
    const bytes_read = try file.readAll(source);
    if (bytes_read != file_size) {
        return Error.readError;
    }

    // Initialize lexer to tokenize the source code
    var lex = try lexer.Lexer.init(allocator, source);
    defer lex.deinit();

    // Initialize parser to build an AST
    var parse = try parser.Parser.init(allocator, &lex);
    defer parser.deinit();

    // Parse the program into an AST
    const program = try parse.parseProgram();
    defer program.deinit();

    // Report any parsing errors & exit if found
    if (parse.errors.items.len > 0) {
        std.debug.print("Parser errors:\n", .{});
        for (parse.errors.items) |err| {
            std.debug.print("    {s}\n", .{err});
        }
        return;
    }

    // Initialize execution environment
    var env = try interpreter.Environment.init(allocator);
    defer env.deinit();

    // Execute program & get the result
    const result = try interpreter.evaluate(program, &env);
    defer result.deinit();
    // NOTE: Result isn't printed to stdout in file mode
}

// Starts an interactive REPL
fn startRepl(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    // Initialize interpreter environment
    var env = try interpreter.Environment.init(allocator);
    defer env.deinit();
    var buffer: [1024]u8 = undefined;

    try stdout.print("lexica REPL - Type 'exit' to quit.\n", .{});

    // Main REPL loop
    while (true) {
        try stdout.print(">> ", .{});

        // Read user input
        const line = (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) orelse break;

        // check for exit command
        if (std.mem.eql(u8, line, "exit")) {
            break;
        }

        // Process the input through lexical analysis
        var lex = try lexer.Lexer.init(allocator, line);
        defer lex.deinit();

        // Initialize parser to build an AST
        var parse = try parser.Parser.init(allocator, &lex);
        defer parse.deinit();

        // Parse program into an AST
        const program = try parse.parseProgram();
        defer program.deinit();

        // Report any parsing errors
        if (parse.errors.items.len > 0) {
            try stdout.print("Parser errors:\n", .{});
            for (parse.errors.items) |err| {
                try stdout.print("    {s}\n", .{err});
            }
            continue;
        }

        // Evaluate the program & handle errors
        const result = interpreter.evaluate(program, &env) catch |err| {
            try stdout.print("Evaluation error: {}\n", .{err});
            continue;
        };
        defer result.deinit();

        // Print evaluation results
        try stdout.print("{}\n", .{result});
    }
}

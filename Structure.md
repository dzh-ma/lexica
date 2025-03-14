lexica/
├── src/
│   ├── main.zig           # Entry point
│   ├── lexer/
│   │   ├── token.zig      # Token definitions
│   │   └── lexer.zig      # Lexical analysis
│   ├── parser/
│   │   ├── ast.zig        # Abstract syntax tree
│   │   └── parser.zig     # Syntax analysis
│   ├── interpreter/
│   │   ├── environment.zig # Variable scope management
│   │   ├── evaluator.zig  # Expression evaluation
│   │   └── builtins.zig   # Built-in functions
│   └── error/
│       └── error.zig      # Error handling
├── tests/                 # Test suite
│   ├── lexer_tests.zig
│   ├── parser_tests.zig
│   └── interpreter_tests.zig
├── examples/              # Example lexica programs
│   ├── hello_world.lex
│   └── variables.lex
├── docs/                  # Documentation
│   ├── language_spec.md
│   └── tutorial.md
├── build.zig             # Zig build system
└── README.md

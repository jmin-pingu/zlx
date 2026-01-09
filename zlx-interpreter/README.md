# zlx-interpreter: Tree-Walk Interpreter

A tree-walk interpreter implementation of the Lox programming language in Zig.

## Architecture

This interpreter directly evaluates the Abstract Syntax Tree (AST) using the visitor pattern. The execution pipeline:

```
Source Code → Scanner → Parser → Resolver → Interpreter
```

### Components

- **Scanner** (`scanner.zig`): Tokenizes source code into a stream of tokens
- **Parser** (`parser.zig`): Builds an AST from tokens using recursive descent parsing
- **Resolver** (`resolver.zig`): Performs static analysis for variable resolution and closure support
- **Interpreter** (`interpreter.zig`): Evaluates the AST using the visitor pattern
- **Environment** (`environment.zig`): Manages variable scopes via environment chaining

### Primitives

- `token.zig` / `token_type.zig`: Token representation
- `expr.zig`: Expression AST nodes
- `stmt.zig`: Statement AST nodes
- `object.zig`: Runtime value types (numbers, strings, booleans, nil, functions, classes, instances)

### Callables

- `function.zig`: User-defined functions with closure support
- `class.zig`: Class definitions with inheritance
- `instance.zig`: Class instances with property access
- `native.zig`: Built-in functions (`clock`, `panic`)

## Building & Running

```bash
zig build
./zig-out/bin/zlx <file.lox>   # Run a file
./zig-out/bin/zlx              # Start REPL
```

# zlx-bytecode: Stack-Based Bytecode VM

A bytecode compiler and virtual machine implementation of the Lox programming language in Zig.

## Architecture

This implementation compiles source code to bytecode instructions, then executes them on a stack-based virtual machine. The execution pipeline:

```
Source Code → Scanner → Compiler → Bytecode Chunk → VM
```

### Components

- **Scanner** (`scanner.zig`): Tokenizes source code into a stream of tokens
- **Compiler** (`compiler.zig`): Compiles tokens to bytecode using Pratt parsing for expressions
- **Chunk** (`chunk.zig`): Container for bytecode instructions and constants
- **VM** (`vm.zig`): Stack-based virtual machine that executes bytecode
- **Value** (`value.zig`): Runtime value representation

### Bytecode Operations

The VM supports the following opcodes:

| Opcode | Description |
|--------|-------------|
| `OP_CONSTANT` | Push constant onto stack |
| `OP_TRUE/FALSE/NIL` | Push literal values |
| `OP_ADD/SUBTRACT/MULTIPLY/DIVIDE` | Arithmetic operations |
| `OP_NEGATE/NOT` | Unary operations |
| `OP_EQUAL/GREATER/LESS` | Comparison operations |
| `OP_PRINT` | Print top of stack |
| `OP_POP` | Pop value from stack |
| `OP_DEFINE_GLOBAL` | Define global variable |
| `OP_GET_GLOBAL/SET_GLOBAL` | Access global variables |
| `OP_GET_LOCAL/SET_LOCAL` | Access local variables |
| `OP_JUMP_IF_FALSE` | Conditional jump |
| `OP_LOOP` | Loop back |
| `OP_RETURN` | Return from function |

## Building & Running

```bash
zig build
./zig-out/bin/zlx <file.lox>   # Run a file
./zig-out/bin/zlx              # Start REPL
```

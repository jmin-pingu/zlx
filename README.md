# zlx: A Zig Implementation of the Lox Programming Language

zlx is an educational implementation of the [Lox programming language](https://craftinginterpreters.com/) written in Zig. This project includes two distinct implementations:

- **zlx-interpreter**: A tree-walk interpreter that directly evaluates the AST
- **zlx-bytecode**: A stack-based bytecode virtual machine

## Building

Both implementations use Zig's build system. Requires Zig 0.11+.

```bash
# Build the tree-walk interpreter
cd zlx-interpreter
zig build

# Build the bytecode VM
cd zlx-bytecode
zig build
```

## Running

```bash
# Run a file
./zig-out/bin/zlx <filename.lox>

# Start the REPL
./zig-out/bin/zlx
```

## Language Features

### Data Types
- **Numbers**: 64-bit floating point (`42`, `3.14`)
- **Strings**: UTF-8 strings (`"hello, world"`)
- **Booleans**: `true`, `false`
- **Nil**: `nil`

### Variables
```lox
var name = "zlx";
var uninitialized;  // defaults to nil
name = "updated";   // reassignment
```

### Control Flow
```lox
// if-else
if (condition) {
    print "yes";
} else {
    print "no";
}

// while loop
while (x < 10) {
    print x;
    x = x + 1;
}

// for loop
for (var i = 0; i < 10; i = i + 1) {
    print i;
}
```

### Functions
```lox
// named functions
fun greet(name) {
    print "Hello, " + name + "!";
}

// anonymous functions (lambdas)
var double = fun (x) { return x * 2; };

// closures
fun makeCounter() {
    var count = 0;
    return fun () {
        count = count + 1;
        return count;
    };
}
```

### Classes and OOP
```lox
class Animal {
    init(name) {
        this.name = name;
    }

    speak() {
        print this.name + " makes a sound";
    }
}

// inheritance
class Dog < Animal {
    speak() {
        print this.name + " barks";
    }

    fetch() {
        print this.name + " fetches the ball";
    }
}

var dog = Dog("Rex");
dog.speak();  // "Rex barks"
```

### Operators
- **Arithmetic**: `+`, `-`, `*`, `/`
- **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Logical**: `and`, `or`, `!`
- **Unary**: `-` (negate), `!` (not)

### Built-in Functions
- `print` - Output to stdout
- `clock()` - Returns current time in seconds

## Project Structure

```
zlx/
├── zlx-interpreter/          # Tree-walk interpreter
│   └── src/
│       ├── main.zig          # Entry point
│       ├── scanner.zig       # Lexical analysis
│       ├── parser.zig        # Syntax analysis (AST generation)
│       ├── resolver.zig      # Static analysis & variable resolution
│       ├── interpreter.zig   # AST evaluation
│       ├── environment.zig   # Variable scope management
│       └── primitives/       # Core types (tokens, expressions, etc.)
│
└── zlx-bytecode/             # Bytecode VM
    └── src/
        ├── main.zig          # Entry point
        ├── scanner.zig       # Lexical analysis
        ├── compiler.zig      # Bytecode generation (Pratt parsing)
        ├── chunk.zig         # Bytecode container
        ├── vm.zig            # Stack-based virtual machine
        └── value.zig         # Runtime value representation
```

## Implementation Details

### Tree-Walk Interpreter
The interpreter uses the visitor pattern to traverse and evaluate the AST directly. The execution pipeline is:

```
Source → Scanner → Parser → Resolver → Interpreter
```

### Bytecode VM
The VM compiles source code to bytecode instructions, then executes them on a stack-based virtual machine. The pipeline is:

```
Source → Scanner → Compiler → Bytecode Chunk → VM
```

## Grammar

### Statements

```
program        -> declaration* EOF ;

declaration    -> classDecl
                  | funDecl
                  | varDecl
                  | statement ;

classDecl      -> "class" IDENTIFIER ( "<" IDENTIFIER )?
                  "{" function* "}" ;

funDecl        -> "fun" function ;
function       -> IDENTIFIER "(" parameters? ")" block ;
parameters     -> IDENTIFIER ( "," IDENTIFIER )* ;

varDecl        -> "var" IDENTIFIER ( "=" expression )? ";" ;

statement      -> exprStmt
                  | forStmt
                  | ifStmt
                  | printStmt
                  | returnStmt
                  | whileStmt
                  | block ;

returnStmt     -> "return" expression? ";" ;
forStmt        -> "for" "(" ( varDecl | exprStmt | ";" )
                  expression? ";"
                  expression? ")" statement ;
whileStmt      -> "while" "(" expression ")" statement ;
ifStmt         -> "if" "(" expression ")" statement
                  ( "else" statement )? ;
block          -> "{" declaration* "}" ;
```

### Expressions

```
expression     -> assignment ;
assignment     -> ( call "." )? IDENTIFIER "=" assignment
                  | logic_or ;
logic_or       -> logic_and ( "or" logic_and )* ;
logic_and      -> equality ( "and" equality )* ;
equality       -> comparison ( ( "!=" | "==" ) comparison )* ;
comparison     -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           -> factor ( ( "-" | "+" ) factor )* ;
factor         -> unary ( ( "/" | "*" ) unary )* ;
unary          -> ( "!" | "-" ) unary | call ;
call           -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
arguments      -> expression ( "," expression )* ;
primary        -> NUMBER | STRING | "true" | "false" | "nil"
                  | "(" expression ")"
                  | IDENTIFIER
                  | "this"
                  | "super" "." IDENTIFIER
                  | "fun" "(" parameters? ")" block ;
```

## TODO

- [ ] Add test suite (unit + integration tests)
- [ ] Implement vtable structure for interfaces
- [ ] Improve memory management
- [ ] Add multithreading syntax (Go-style)

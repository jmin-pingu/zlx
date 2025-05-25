# zlx: a Zig implementation of the lox programming language

## Build Details

## TODO

- [X] KEY: Need to improve error tooling, messaging, and handling.
- [X] KEY: need to reason about interfaces (blog post incoming
- [X] Reorganize `Callable`. Combine `function.zig`, `callable.zig` (`GenericCallable`), and
  instances logic
- [ ] PRIORITY: think how to add test cases (both unit + integration), **Allocate a day to handle this task**.
  - [ ] Implement test suite in target language via native functions (`zlox`)
- [X] add closures
- [X] add anonymous functions
- [ ] add vtable structure
- [ ] Need to improve overall structure (naming, modules, and organization).
- [ ] Need to better actively think about memory management
- [ ] Need to better think about runtime v. comptime
- [ ] Figure out Zig build system.

## Features
- [ ] add multithreading syntax like `go` in `Golang`

## Grammar

Grammar for Statements

```{markdown}
program        -> declaration * EOF ; 

declaration    -> classDecl
                  | funDecl 
                  | varDecl 
                  | statement ;

classDecl      -> "class" IDENTIFIER "{" function* "}" ;
function       -> IDENTIFIER "(" parameters? ")" block ; 
parameters     -> IDENTIFIER ( "," IDENTIFIER  )* ; 
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
                     
forStmt        -> "for" "(" ( varDecl | exprStmt | ; )
                  expression? ";"
                  expression? ")" break_statement ;

whileStmt      -> "while" "(" expression ")" break_statement ;

TODO: rethink structure of `break` in the grammar
breakStatement -> break 
                  | exprStmt
                  | forStmt
                  | breakIfStmt
                  | printStmt
                  | returnStmt
                  | whileStm 
                  | breakBlock ;

breakIfStmt    -> if "(" expression ")" breakStatement
                  ( "else" breakStatement )? ;  

breakBlock     -> "{" breakDecl* "}" ;

breakDecl      -> funDecl
                  | varDecl 
                  | breakStatement ;

ifStmt         -> if "(" expression ")" statement
                  ( "else" statement )? ;  

block          -> "{" declaration* "}" ;
```

Grammar for Expressions

```{markdown}
expression     -> assignment ;
assignment     -> ( call "." )? IDENTIFIER "=" assignment 
                  | logic_or 
logic_or       -> logic_and ( "or" logic_and )* ;
logic_and      -> equality ( "and" equality )*;
equality       -> comparison ( ( "!=" | "==" ) comparison )* ;
comparison     -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           -> factor ( ( "-" | "+" ) factor )* ;
factor         -> unary ( ( "/" | "*" ) unary )* ;
unary          -> ( "!" | "-" ) unary
                  | call;
call           -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
arguments      -> expression ( "," expression )* ;
primary        -> anonymous | NUMBER | STRING | "true" | "false" | "nil"
                  | "(" expression ")" ;

anonymousExpr  -> "fun" anon ;
anonymous      -> "(" parameters? ")" block ;
parameters     -> IDENTIFIER ( "," IDENTIFIER )* ;
```


2023 Fall

Overview
This OCaml Interpreter project is an implementation of an interpreter for a high-level programming language. The interpreter is capable of parsing source code written in the specified language, building an abstract syntax tree (AST), and executing the corresponding compiled code.

The high-level language supported by this interpreter includes various features such as:

Arithmetic and logical expressions
Variables
Functions with parameters
Recursive functions
Let-bindings
Conditional statements (if-then-else)
Sequence of expressions
Trace statements
Basic error handling for unbound variables
Project Structure
Parser
The project includes a parser for the high-level language, which translates source code into an abstract syntax tree (AST). The parser is implemented using OCaml and consists of various parsing functions for different language constructs.

Abstract Syntax Tree (AST)
The AST represents the structure of the parsed source code. It is defined using OCaml data types, with each type corresponding to a different language construct. The AST is used as an intermediate representation before the code is compiled and executed.

Compiler
The compiler translates the AST into executable code. It traverses the AST, generating a sequence of instructions that can be executed by the interpreter. The compiler handles various language constructs, including arithmetic operations, function definitions, let-bindings, and conditional statements.

Interpreter
The interpreter takes the compiled code produced by the compiler and executes it. It follows the instructions generated during the compilation process, updating the state and producing the final result of the program.

Usage
To use the interpreter, provide source code written in the high-level language as input. The interpreter will parse, compile, and execute the code, producing the final output.
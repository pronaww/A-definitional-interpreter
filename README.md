# A definitional interpreter

In this assignment, you will define the abstract syntax (data type exp) and a definitional interpreter eval for a simple arithmetic and boolean calculation language.


The expressions in the language are of the following forms

    Integer constants, 
    Unary arithmetic operations: abs, (and any other sensible ones you can think of),
    Identifiers, represented as (alphanumeric) strings
    binary operations: + (addition), - (subtraction), * (multiplication), div, mod, ^ (exponentiation)
    Boolean constants: T and F
    Unary boolean operation: not
    binary boolean operations:  /\ (and), \/ (or), -> (implies)
    Comparison operators: = (equal) , > (greater than), < (less than) , >= (greater or equal), <= (less or equal) on integer expressions
    n-tuples for each n > 2
    Projection operators proj(i,n) which project the ith element of an n-tuple.


Assume all inputs are of proper type (we will study type-checking later).  Define a suitable data type answer.


eval: exp -> answer.

Next, define a suitable set of opcodes for a stack-and-table machine to evaluate this language and define a compiler for this language to sequences of these opcodes.

compile: exp -> opcode list

Third, define the stack machine execution functions, which takes a sequence of opcodes and executes them starting from a given stack and table.

execute: stack * table * opcode list -> answer


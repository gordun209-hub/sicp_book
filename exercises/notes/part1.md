<!--toc:start-->

- [title: notes from part1](#title-notes-from-part1)
- [Elements of Programming](#elements-of-programming)
- [Evaluating Combinations](#evaluating-combinations)
- [primitives](#primitives)
- [Compound Procedures](#compound-procedures)

<!--toc:end-->

# title: notes from part1

## Elements of Programming

A powerful programming language is more than just a means for instructing
a computer to perform tasks. The language also serves as a framework
within which we organize our ideas about processes.
Thus, when we describe a language, we should pay particular attention to the
means that the language provides for combining simple ideas to form more
complex ideas. Every powerful language has three mechanisms for accompllishing
this:

1. **primitive expressions**, which represent the simplest entities the
   language is concerned with,

1. **means of combination**, by which compound elements are built from simpler
   ones, and

1. **means of abstraction**, by which compound elements can be named and
   manipulated as units.

expressions:

```racket
486
```

prints 486

## Evaluating Combinations

1. Evaluate the subexpressions of the combination.

1. Apply the procedure that is the value of the leftmos subexpression
   to the arguments that are the values of the other subexpressions(the operands)

## primitives

1. the values of numerals are the numbers that they name,

1. the values of built-in operators are the machine instruction
   sequences that carry out the corresponding operations, and

1. the values of other names are the objects associated with those names in
   environment

## Compound Procedures

1. Numbers and arithmetic operations are primitive data and procedures
1. Nesting of combinations provides a means of combining operations
1. Definitions that assicate with values provide a limited means of abstraction.

```racket
(define (square x) (* x x))
```

We can understand this in the following way:

```racket
(define  (square      x) (     *     x      x))
```

To      square   something multiply it  by itself.

General form of a procedure definition is
;;(define ( \< name> \< formal parameters >)
    <body>)



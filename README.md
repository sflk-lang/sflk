<img src="/logo/sflk-logo-color.svg" align="right" width="20%" alt="SFLK logo" />

# SFLK programming language

## Introduction

*SFLK* is an interpreted programming language. This repository contains the
reference implementation.
SFLK doesn't focus on being fast, a lot of languages are fast already. Instead,
it tries new things, in the hope of being intresting (or even useful).
Enjoy!

## Presentation

The language is very unstable as it is still in very early development.
What is written here may not be accurate.

```sflk
# Comments are blocks delimited by hashes #
#### Or by any number
of successive #s ####
```

There is no statement separator and whitespace is not significant.
If one is to indent, one is encourged to use the indentation method one loves.

```sflk
pr "uwu" nl
```

These are two statements:
a print statement that is the `pr` keyword and an expression
and a newline statement that only consists of the `nl` keyword.

```sflk
     pr
"uwu"nl
```

These are exactly the same as above.

`"uwu"` is a string literal that get parsed to a string object.
There are other types, although more are coming soon.

```sflk
ev 8 # Integer type #
ev {pr "uwu" nl} # Code block type #
# Boolean type is coming soon #
# List type is coming soon #
```

The evaluation statement consists of the keyword `ev` and an expression.
It evaluates the expression.
Note that the line introducing the code block doesn't prints "uwu" because
evaluating the code block literal expression only create the code block object
and doesn't execute it. The do statement does.

```sflk
do {pr "uwu" nl}
```

Code blocks are objects, and like any other objects they can be values assigned
to variables.

```sflk
x < {pr "u"}
y < {pr "w"}
do x + y + x + {nl} # Prints the string "uwu" and a newline #
```

Assignment statement begins with a target (assign to what),
then the statement operator `<`, and then an expression.
Code blocks can be concatenated with the `+` binary operator.

Note that `{pr} + {"uwu"}` doesn't evaluate to the code block `{pr "uwu"}`,
as the blocks are created before being concatenated and concatenation doesn't
try to merge incomplete statements together into a valid statement.

An expression that contains binary operators is a chain.
Binary operators actually pair up with their right argument to form chops
(chain operations) that are applied sequentially to the chain accumulator,
there is no such thing as operator precedence in SFLK.

```sflk
pr 8 +1 /3 -1 *10 nl
#  8  9  3  2  20 #
```

Putting a white space on the left and not on the right of binary operators
makes the chain structure very clear and is a good idea.
In the above example, `8 +1 /3 -1 *10` is a chain expression, its initial
value is `8` and the chops are `+1`, `/3`, `-1` and `*10`. Evaluating this
expression is done by initialising the chain operator to the initial value `8`
and then applying the chops to it in order.

It is possible to use `()` to organize expression better.

```sflk
pr 8 *(1+1) nl # Prints 16 #
```

The binary operator `>` should be paired with an expression that evaluates to
a code block, which will be executed in a new context with the variable `v`
initially holding the chain accumulator value, and at the end of this execution
the chain accumulatro is set to the value of `v`.

```sflk
print < {pr v nl}
ev 4 +4 >{v<5} >{v<v+2} >print
#  4  8      5        7      7 #
```

There is a list of operators and their effects on different types down below.

## Statements

### Nop

The *nop* statement consists of the `np` keyword and doesn't do anything.

### Print

The *print* statement consists of the `pr` keyword followed by an expression,
the evaluation of which is printed without trailing newline.

### Newline

The *newline* statement consists of the `nl` keyword and prints a newline
character.

### Evaluate

The *evaluate* statement consists of the `ev` keyword followed by an
expression, which is evaluated. The result of the evaluation is discarded.
It is to be noted that an expression evaluation may have intended side effects.

### Do

The *do* statement consists of the `do` keyword followed by an expression.
The expression is evaluated in a value that should be a code block that is
then executed in a new execusion context.

### Do here

The *do here* statement is the same as the *do* statement, with the `dh`
keyword being used instead of the `do` keyword, and the code block is executed
in the current execusion context instead of in a new one.

### If

The *if* statement consists of the `if` keyword followed by an expression,
and then the optional following extensions *then* and *else* in any order:
The *then* extension consists of the `th` keyword followed by a statement and
the *else* extension consists of the `el` keyword followed by a statement.
The optional *then* extension's statement is called the *then branch*, and
the optional *else* extension's statement is called the *else branch*.

The expression is evaluated. If it was evaluated in a truthy value,
then the *then branch* is executed if it exists,
else the *else branch* is executed if it exists.

Example:

```sflk
if x
th pr "true"
el pr "false"
```

### Assign

The *assign* statement consists of a target (like a variable name) flollowed by
a *to left statement operator* `<` followed by an expression.
The expession is evaluated and the target is assigned the result.

Examples:

```sflk
x < 8
uwu < "owo"
```

## Binary operators

### Plus `+`

| accumulator type | right type | effect
|:----------------:|:----------:| ------
| integer          | integer    | Arithmetic addition
| string           | string     | String concatenation
| code block       | code block | Code block concatenation

### Minus `-`

| accumulator type | right type | effect
|:----------------:|:----------:| ------
| integer          | integer    | Arithmetic substraction

### Star `*`

| accumulator type | right type | effect
|:----------------:|:----------:| ------
| integer          | integer    | Arithmetic multiplication
| string           | integer    | String repetition
| code block       | integer    | Code block repetition

### Slash `/`

| accumulator type | right type | effect
|:----------------:|:----------:| ------
| integer          | integer    | Arithmetic euclidean division
| string           | string     | Count non-overlaping occurences of right in left

### To right `>`

| accumulator type | right type | effect
|:----------------:|:----------:| ------
| any type         | code block | Execute right with left as `v`, then sets to `v`

# Contribute

If you want to contribute in any way, please feel free to do so ^^.

Note that there is a Discord server dedicated to
the *development and use* of SFLK
(how to get in there? we don't know haha,
maybe ask an invite some way or something).

<img src="/logo/sflk-logo-color.svg" align="right" width="20%" alt="SFLK logo" />

# SFLK programming language

The language is very unstable as it is still in very early development.
The examples provided here may not be accurate.

```sflk
# Comments are blocks delimited by hashes. #
```

There is no statement separator
and whitespace is not significant.

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
ev 8 # Integer type. #
ev {pr "uwu" nl} # Code block type. #
# Boolean type is coming soon. #
# List type is coming soon. #
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
do x + y + x + {nl} # Prints the string "uwu" and a newline. #
```

Assignment statement begins with a target (assign to what),
then the statement operator `<`, and then an expression.
Code blocks can be concatenated with the `+` binary operator.

Note that `{pr} + {"uwu"}` doesn't evaluate to the code block `{pr "uwu"}`,
as the blocks are created before being concatenated and concatenation doesn't
try to merge incomplete statements together into a valid statement.

TODO: write about expressions, chains, chops, operations on strings, and `>`.

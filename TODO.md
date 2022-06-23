
# SFLK development TODO list and ideas

Here are listed ideas of ameliorations to SFLK.

## Language features

### Variables and contexts

Variable could be declared with the following syntax:

```sflk
x! < 8
```

On the left of `<` there should be a *target expression*, such as a variable name. The new thing here would be a postfix unary target-expression operator `!`. `x!` would be "evaluated" as the target `x`, but with the side effect of declaring `x` in the current context.

Variables must be declared to be accessed. If an access (read or write) is performed on a variable that is not declard in the current context, then the access is performed by a signal that goes toward the root in the context tree, asking for each context along the way if it has the accessed variables. If a context does have the variable declared, then the access is performed in the context that has the variable, and execution continues. If the signal reaches the interpreter past the root, then an error message is printed or something.

That would mean that contexts can attempt to do stuff to contexts that they visit if they are not intercepted before.

Example of how cool that would be:

```sflk
x! < 2
do {
	x! < 6
	do {
		do {
			x < 8
			do {
				do {
					pr x nl
				}
			}
		}
	}
}
pr x nl
```

That would print `8` and then `2`. Here there would only be 2 variables called `x`, the assignment `x < 6` assigns to the `x` variable that was initialized to `6`, the first `pr x` statement also accesses this variable. So basically declaring a variable means it is local to the current context, and shadows all the variables with the same name of the parent contexts.

Imho, this is so much more better than python's `global x`, `nonlocal x`, and wierd behavior in which reading a variable in a function reads the global variable but only if it was not written to before in the function iirc.

#### Note about `>`

In `ev 8 > {v < v*2}`, `v < v*2` is run in a sub-context in which `v` is declared and assigned `8`, so that `>` keeps working as intended.

### Comparisons

Instead of having comparions operators like `<` and `>` (which are already taken anyway), SFLK coule instead provide the following syntax:

```sflk
if od 4,, x, 8
th pr "x is in {4, 5, 6, 7, 8}" th nl

if os 4,, x, 8
th pr "x is in {5, 6, 7}" th nl
```

The new things here would be the unary operators `od` and `os`. `od` would expect a list and evaluate to a value whose truthyness is true iff the list is OrdereD (duplicates allowed) and `os` would be the same but with duplicates disallowed (Ordered but Strictly).

### "Lambda captures"

Here is the problem:

```sflk
x! < 8
block! < {
	x! < 42
	v < {
		v < x
	}
}
pr () > (() > block) nl
```

Here, `block` returns a block (let it be **B**) that returns the value of `x` (which will depend on the contexts at the moment **B** is executed). But what if we wanted `block` to return a block that returns an "hardcoded" `42` (or whatever the "inner" x is), or even an "hardcoded" `8` (or whatever the "outer" x is at the time `block` is set, regardless of what it is at the moment it is run later)?

To provide a way to do this, the following syntax could be added to SFLK:

```sflk
x! < 8
block! < {
	x! < 42
	marker! < ()
	v < {
		v < x~marker
	}
}
pr () > (() > block) nl
```

The new thing here would be the `~name` syntax. Here is what that would mean and do:

First, it would terminate an expression the same way `.` does. But it would also mark the exception it terminates in a special way. Mm it is quite annoying to explain.

In the above example, there are 2 block literals:
```sflk
{
	x! < 42
	marker! < ()
	v < {
		v < x~marker
	}
}
```
and
```sflk
{
	v < x~marker
}
```

Lets call the big one **A** and the small one **B**. **A** contains **B**.

Usually, a literal is an expression that evaluates to a constant whose value is what the literal represent. Here, `~marker` messes up the concept of literal by making **B** *directly incomplete* (as it contains an expression that is marked by a `~name` thing) (**B** is also *incomplete*). **A** is just *incomplete* as it contains an expression that contains a block literal that is *incomplete*.

Each time an incomplete block literal expression is evaluated, all the `~name`-marked expressions contained in itself recursively (thus also in the block literals it contains, etc.) are attempted to be replaced by normal expressions. For an expression `E~name`, its evaluation is attempted as follows:

- We search a context in which `name` is a defined variable, starting in the current context and going towards the root until it is found.
- If such context is not found (i.e. the search reaches the interpreter past the root), then the replacement fails (the expression remains `E~name`).
- If such context is found, then `E` is evaluated in that context, and the result of this evaluation is what replaces `E~name`.

So, in the example with **A** and **B**, it would go as follows:

- `x` is declared and is assigned the value `8`, whatever.
- `block` is declared and is assigned the value of the evaluation of the expression **A** that goes as follows:
  - **A** contains **B** that contains `x~marker`, we will this attempt to replace `x~marker` by a normal expression.
  - We search a context in which `marker` is declared.
  - We don't find such context (there is only one context and it only contains `x` and maybe `block` so far).
  - The replacement attempt fails.
- Very well, then `pr () > (() > block) nl` is executed. It runs `block` (**A**) to get the block **B** that it returns, then it runs **B** to get the value that it returns, and print that with a newline.
- While it runs **A** (in a sub-context):
  - `v` is declared and is assigned the value `()` (that is the doing of executing **A** with `() >`).
  - `x` is declared and is assigned the value `42`.
  - `marker` is declared and is assigned the value `()`.
  - `v` is assigned the result of the evaluation of `{v < x~marker}` (**B**), here goes its evaluation:
    - This is an incomplete block literal as it still contains `x~marker`, so we attempt to replace it.
    - We search a context in which `marker` is declared.
    - Oh, we find one (the current context)!
    - We evaluate the expression `x` in this context.
    - It evaluates to `42`.
    - `x~marker` is thus replaced by an expression that evaluates to the contant `42`.
    - There are no more things to replace, the final block is `{v < 42}`.
  - As `v` is `{v < 42}`, this is what is returned.
- The returned block, `{v < 42}`, is run, and the value it returned, `42`, is printed, followed by a newline.

With this, the initial problem is solved. Even with 50 levels of nesting of block literals, these wierd expressions can capture whatever we want in arbitrary outer layers (by placing declarations like `marker! < ()` in the layers of the contexts we want to capture stuff from). Even more: the expressions marked with `~name` things can be more rich than just variable names (although stuff like `(1+2)~marker` kind of misses the point).

In the example, if `marker! < ()` was out of the big block literal, then the replacing attempt during the evaluation of the big block literal would have been successful and the final printed value would have been `8` instead of `42`.

#### Note about the evaluation of a non-replaced thing

```sflk
do {
	ev 8~owo
}
```

The above piece of code produces an error if executed as-is, as the evaluation of the block literal cannot replace its `8~owo`, which is then evaluated without having been replaced by a normal expression (and that is illegal for now).

#### Note about the implementation

If could make things simpler if a code block object would separate code from data, for example the literals could be like *Constant 17*, and there would be a table of constants coming with the code.

An entry in this table could either be a normal SFLK object, or a marked expression (like the SIR code that evaluates the expression, no need to store the AST in there) coupled with the marker name.

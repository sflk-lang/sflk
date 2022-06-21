
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

### Comparisons

Instead of having comparions operators like `<` and `>` (which are already taken anyway), SFLK coule instead provide the following syntax:

```sflk
if od 4,, x, 8
th pr "x is in {4, 5, 6, 7, 8}" th nl

if so 4,, x, 8
th pr "x is in {5, 6, 7}" th nl
```

The new things here would be the unary operators `od` and `so`. `od` would expect a list and evaluate to a value whose truthyness is true iff the list is OrdereD (duplicates allowed) and `so` would be the same but with duplicates disallowed (Strictly Ordered).

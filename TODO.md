
# SFLK development TODO list and ideas

Here are listed ideas of ameliorations to SFLK.

## Big language features

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

#### Note on a simpler alternative

Instead of `expression~marker`, we could simply make `variable~` read the variable of the given name in the first context to contain it.

Maybe it is not a good idea, the marker seems important to help be more explicit about what context is used for the capture.

#### Note on more explicit handling of this feature

Instead of attempting replacements implicitely like proposed previously, we could provide an unary operator that attemps the replacements.

**This looks like it is the better decision.**

### Contexts vs general maps

It could also be nice to have maps from anything to anything, it is highly probable that SFLK will get some in one way or another. To avoid confusion between these types, we could call the type discussed in this entry by the name of *context*, and the for futur general-purpose maps *dictionaries*.

### Big ints

Have every number be a normalized big int able to store arbitraly big integer values.

### Big fractions

Have every number be a normalized fraction of big ints so that we can manipulate arbitraly precise rational numbers.

## Everything is a function, a list and a context

### Everything is a function

A list is kind of a function of indices to objects. A context is a function of names to objects. A string is a list of characters so kind of a function there. A number is a list of digits (moreon that later) so also a function.

### Everything is a list

A block is a list of statements.

A number is a list of digits. The base to use can be the number stored in the variable `b`. On that, numbers like *tau* can have an infinite amount of digits, which is not a problem.

### Everything is a context

The idea here is to be able to `cy` anything and get useful information.

For example, `cy`ing a list can put the first element in `f` (front), the last in `b` (back), the length in `l`, etc. `cy`ing a number can put the numerator in `n` and the denominator in `d`, the integral part in `i`, etc. etc.

## Syntax cool stuff

### Everything should be doable with only alphabetical characters and whitespace

Imagine only keywords and variable names. Huge potential for polyglots.

### Everything should be doable with only non-alphanumerc characters

Imagine bs like `&~ $..!: ++, $, ?, ??&--'-().;;<>` etc. and it actually means something. It would be so cool, huge potential for obfuscation.

## Small language features

### Send error signals instead of panicking

Do that.

### Have a signal format dedicated to errors

That, and print them in red in stderr or something.

## Interpreter

### Execution time debugging

With a verbosity similar to the parsing in debug mode, the execution in debug mode should log everything worthy of notice, and everything else, in a colorful and indented way.

### Rust tests

Rust-level tests should cover most of the interpreter logic.

### Reduce the number of warnings to 0

The 50+ warnings at each `cargo run` are pretty annoying.

### Code readability

The code shoud be made more readable and more commented.

### Error messages in panics

No more empty `unimplemented!`s and dangerous `unwrap`s. When the interpreted explodes for some reason, the reason should try its best to be clear to whoever is using the interpreter.

## Standard library

### PDF creation

A PDF creation feature should be provided as an interceptor that intercepts print statements and adapt them to alter an output PDF document containing the formated content of the intercepted print signals.

Not that it should replace LaTeX, but it could be kinda cool to have that in the stdlib, like this is so random yet so facinating.

## Logo

GitHub project embeds seem to work best when given a 1280×640px image to illustrate the repo. One should be drawn with Inkscape or something to fit the size requirement while still being vector graphics. Why not putting the SFLK logo on the left, and writing SFLK on the center and right ?

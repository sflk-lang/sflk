
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

##### Even better

```sflk
a! < {
	x! < .jej > v
	v < v
		+ {pr "uwu"}
		+ {
			pr " "
			pr x~k
			pr " "
		}
		+ {nl}
	v ? .k
}
```

Consider how in this example the `?` operator (can be an other syntax) (with `.k` as its right operand) makes it clear when `x~k` is replaced.

**This looks like it is the better decision.**

### Contexts vs general maps

It could also be nice to have maps from anything to anything, it is highly probable that SFLK will get some in one way or another. To avoid confusion between these types, we could call the type discussed in this entry by the name of *context*, and the for futur general-purpose maps *dictionaries*.

### Real numbers

Have the ability to define numbers with stuff like code that returns the i-th decimal in base b or something, so that they can be used up to any precision. Then make them behave like numbers (stuff like be printed, be multiplied by 2, etc.). Then make a variable like `e` (for error) be used as a parameter for their use that can indicate that their use is only guarenteed up to some precision with the error being less than the content of `e` or something (and of course if `e` is something like `()` then their use must be of infinite precision and printing them hould take an infinite amount of time to terminate for example).

### Even better signals

A print signal does nothing as it goes through contexts and travels toward the root, but a variable writing signal does stuff at each signal it encounters. The way this is made possible by the interpreter is that the name of some signals (e.g. variable writing) is considered a special case by the interpreter. Not very elegant and general.

Not sure how to make it better tho.. Making each signal having blocks of code to run in every contexts it passes through may let interceptors a bit clueless as (for now) there is no way to study what a block of code does or do stuff with it; so a signal could name itself "readvar" but its block of code may actually do more stuff than just reading a variable or something.

TODO: Think about it.

## Everything is a function, a list and a context

### Everything is a function

A list is kind of a function of indices to objects. A context is a function of names to objects. A string is a list of characters so kind of a function there. A number is a list of digits (moreon that later) so also a function.

### Everything is a list

A block is a list of statements.

A number is a list of digits. The base to use can be the number stored in the variable `b`. On that, numbers like *tau* can have an infinite amount of digits, which is not a problem.

### Everything is a context

The idea here is to be able to `cy` anything and get useful information.

For example, `cy`ing a list can put the first element in `f` (front), the last in `b` (back), the length in `l`, etc. `cy`ing a number can put the numerator in `n` and the denominator in `d`, the integral part in `i`, etc. etc.

## Lazy operations when necessary

Doing something like `{pr "a"} * n` with `n` being too large will fail because the star operation actually constructs a block that is the concatenation of `n` copies of the left operand block. This was easy to program, but it makes this operation limited.

A solution would be to make `{pr "a"} * n` construct an object that just contains the left opereand block and the value of `n` and it being a variant form of a block. Like, a block can be a sequence of instruction or a repetition (n times) of a sub-block.

However, this makes everything more complicated. What about `{pr "a"} * n + {pr "b"}`? To handle this case, now a block must also have a variant for a lazy plus (because the left operand given to `+` here is not a sequence of instructions to which the right sequence of instruction can be concatenated to).

This should also apply to strings, lists, etc. Handle indexing correctly. Handle execution of such blocks correctly. And all the other issues that I have not thought about yet.

## Easy loading bar handling

Make it very easy (somehow) to make a long operation or whatever communicate its progress in the form of a loading bar.

This may be doable by introducing a new signal type that an operation emits regularly to communicate its progress, and some interceptor is free to use these to redraw a loading bar. Maybe? Would it be easy to use such thing?

## Syntax cool stuff

### Everything should be doable with only alphabetical characters and whitespace

Imagine only keywords and variable names. Huge potential for polyglots.

### Everything should be doable with only non-alphanumerc characters

Imagine bs like `&~ $..!: ++, $, ?, ??&--'-().;;<>` etc. and it actually means something. It would be so cool, huge potential for obfuscation.

## Small language features

### Block statements

New token `.{` (no whitespace between the `.` and the `{`) that opens a block that is closed with `}` (just like the regular `{`) but instead of producing a block of code literal that is an expression, it produces a block statement that is a statement. When executed, its effect is to execute the sequence of statements it contains. It would allow to replace stuff like `if x th dh { ... }` by `if x th .{ ... }` which is clearly more elegant.

Note: For some time I thought about using `( ... )` for block statements, but now I don't think this is a good idea. It seems better to stick to the implicit rules that say "parenthesis are for expressions" and "code is in curly-braces".

### Chop-like assign

New token `.>` (no whitespace between the `.` and the `>`) that behaves *similarly* to a chop (chain operator) and does what `<` allows to do. Consider `ev 2 + 6 .> x .> y`, well this should assign 8 to `x` and then to `y`. `.>` is not a chop tho (as what is on the right of it is not an expression but a *target expression*). Note that `ev 2 + 6 .> x! .> y!` should be valid and do what we expect it to do (declare `x` and assign 8 to `x` then declare `y` and assign 8 to `y`).

### `rs` in the `em` statement should accept `x!`

This. All target expressions should be accepted where a target expression is expected. The fact that it is currently not the case is kinda a bug.

### Some equivalent of `repr`

In Python there is a `repr` function (or whatever calls `__repr__`) that is different from `str` (that calls `__str__`). `repr` is supposed to return a string that is a Python expression that evaluates into the object. SFLK should have something like that.

### Send error signals instead of panicking

Do that. Also, use a dedicated signal type instead of just print signal. Use the difference in signal type to print them in red in stderr or something.

## Interpreter

### Object stack

Replace the object stack by a stack of something else. The goal here is to make the stack better represent the stack of nested scopes of operations that we are in, and contain relevant operation information in a better way. For example, a loop will push a loop scope or somthing that holds the loop counter and all the loop information that are needed to make it work (no more loop counter pushed as an SFLK number object).

More generally, when an operation involves having an instruction pushing some information on the stack, and having an other instruction pop this information later (with every instruction inbetween expected to not interact with this information) then making this information into a dedicated type and type checking every pop makes sense (easier debgging when poping too much or not enough, etc.).

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

## Built-in cool features

### Assembly and machine-code generation and execution

Allow an SFLK progam to produce machine code (with the help of some assembly pieces provided by the standard library or something) and to run the produced machine code by directly jumping in it or something.

This would be very unsafe, but very fun as it will bring very low level considerations into funcky SFLK code!

### Graphics window

Allow an SFLK program to create windows, recieve events and draw on the surface, etc.

Beware the dependencies! Maybe make it opt-out with cargo feature and `#[cfg(stuff)]` attributes. Also, avoid the `sdl2` crate (as it requires SDL2 dev lib and is a mess to make it link statically). The dependency MUST link statically and not require any setup from someone who just want to clone and compile SFLK with all the features (thus a standalone crate is required).

How about [notan](https://crates.io/crates/notan)? It seem to be the first crate in the #windowing cathegory that does not says that installing it requires anything else than adding it via cargo like a normal simple crate. See some [examples](https://nazariglez.github.io/notan-web/) maybe.

### Sound

Allow an SFLK program to play sound. Something that could be really cool would be to allow to give a list of bytes and give it directly to the system mixer. Then the stdlib and the programs can synthesize sound and mix and all.

Beware the dependencies! Maybe make it opt-out with cargo feature and `#[cfg(stuff)]` attributes. Also, avoid the `sdl2` crate (as it requires SDL2 dev lib and is a mess to make it link statically). The dependency MUST link statically and not require any setup from someone who just want to clone and compile SFLK with all the features (thus a standalone crate is required).

## Standard library

### PDF creation

A PDF creation feature should be provided as an interceptor that intercepts print statements and adapt them to alter an output PDF document containing the formated content of the intercepted print signals.

Not that it should replace LaTeX, but it could be kinda cool to have that in the stdlib, like this is so random yet so facinating.

## Logo

GitHub project embeds seem to work best when given a 1280×640px image to illustrate the repo. One should be drawn with Inkscape or something to fit the size requirement while still being vector graphics. Why not putting the SFLK logo on the left, and writing SFLK on the center and right ?

## Architecture

Take inspiration from cool stuff like [Rome](https://github.com/rome/tools) to refractor the parsing stage into something really solid (and maybe even usable by a language server). 

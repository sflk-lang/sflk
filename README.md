<img src="/logo/sflk-logo-color.svg" align="right" width="20%" alt="SFLK logo" />

# SFLK programming language

## Introduction

*SFLK* is an interpreted programming language. This repository contains the reference implementation.
SFLK doesn't focus on being fast, a lot of languages are fast already. Instead, it tries new things, in the hope of being interesting. 20% serious, 80% weird stuff.
Enjoy!

## Presentation

The language is very unstable as it is still in very early development.
Many many essential features are still missing.
What is written here may not have beed updated properly (but all that is described here works or have worked in somepast commit, SFLK is not vaporware).

### Syntax

#### The basics

```sflk
pr "Hello world!" nl
```

This SFLK program contains two statements: a print statement and a newline statement. The whitespace is not significant in any way, except for separating tokens when necessary. There is no statement separator, which makes understanding poorly formatted SFLK programs
pretty hard for one unknowing of all the statement syntaxes.

An SFLK program is basically a sequence of statements, some of which may contain other statements or even generate code at run-time to allow for rich control flow. Most statements begin with a keyword (such as `pr`), most keywords are 2 characters long.

Some statements expect stuff to follow some keywords, such as the print statement that expects an expression after its `pr` keyword. The newline statement does not.

#### Expressions

```sflk
pr 6 + 2 nl
```

This piece of code prints `8` as the expression `6 + 2` is evaluated during the execution of the statement `pr 6 + 2`.

Binary operators such as `+` do not have a precedence, expressions such as `1 + 2 * 3 / 4 - 5` are really understood as `((((1 + 2) * 3) / 4) - 5)` and this is why a good formatting practice is to write these as `1 +2 *3 /4 -5`. Parenthesis are supported to allow some freedom on the shape of expressions.

#### Comments

```sflk
# Comments are blocks delimited by hashes #
#### Or by any number
of successive #s ####
```

#### Variables

```sflk
x! < "So long"
x < x + " gay "
pr x + "Bowser" nl
```

Here, `x` is a variable that is being assigned a string object, and again, to finally be used in an expression that is printed.

The `name < expression` syntax is the one of the assignment statement. The expression is evaluated and the result is stored in the variable of the given name. The `name! < expression` (note the `!`) declares the variable before doing the assignment, which is convenient for a first assignment since variables must be declared before being assigned values.

To discard the expression final value instead of storing it, the evaluation statement can be used instead with the syntax `ev expression`.

#### Code blocks

```sflk
do {pr "uwu" nl}
```

This piece of code prints `uwu` as one might expect. The code block `{pr "uwu" nl}` is actually a code block literal that is an expression, as code blocks are objects just as integers and strings.

```sflk
x! < {pr "uwu"}
x < x + {nl}
do x
```

This also prints `uwu` followed by a newline, as the code block executed in the do statement is the concatenation of the two literals `{pr "uwu"}` and `{nl}`.

It is to be noted that `{pr "uwu"} + {nl}` is valid, but `{pr} + {"uwu" nl}` is not. `{pr}` is invalid syntax as a print statement expects an expression, and `{"uwu" nl}` is invalid syntax as `"uwu"` is not a statement. To be able to concatenate partial invalid code pieces as if they were strings, then just concatenate strings like so: `"pr" + "\"uwu\" nl"`, the resulting sting `"pr\"uwu\" nl"` happens to be valid SFLK code and can be executed as code when given to a do statement (or in any other place where a code block can be given). Beware of potential token concatenation though (`"pr x" + "nl"` evaluates into `"pr xnl"`, you see the issue).

One might think of code blocks as functions or procedures or whatever. They are just pieces of code really. SFLK does not care (that much) about debugability or readability and intends to allow as much dynamic bullshit as possible, as demonstrated in the following section:

#### `>` operator

```sflk
double! < {v < v *2}
```

Here, `double` is a code block that only consists of the statement `v < v *2`. The variable `v` is special: it is the variable used to pass arguments to a code block, as well as to return a value.

```sflk
pr 4 >double nl
```

The evaluation of `4 >double` goes like this: the value `double` is evaluated, as well as the value `4`. A new context (more on that later) is created for the execution of the code block. In this context, the variable `v` is initialized to `4` before the code block is executed. After the code block is executed, the value of `v` (here `8`) is extracted from the that context (that is discarded) to be the value to which the expression `4 >double` evaluates to. So in the end `8` is printed.

Now for a demonstration of how deranged is SFLK's dynamism, consider the following:

```sflk
quad! < double >double
pr 4 >quad nl
```

Yes, that is right, `16` is printed. `double >double` takes advantage of the fact that `*2` is a valid operation on a code block (simply duplicating its sequence of statements). That produces a new code block that performs the action of `double` two times. Thanks to the design choice of making all the input and output of a block pass via the `v` variable, block concatenation has the same effect as composition, and the block assigned to `quad` makes sense: it doubles doubly (i.e. it quadruples).

#### Unary operators

```sflk
pr -1      nl # -1 #
pr -1+1 +1 nl # -3 #
pr -1+1.+1 nl # -1 #
```

Although `-` can be a binary operator, it can also be a unary operator. Since it only takes one argument (the expression on its right) then nothing is expected on its left. However, when does the expression it takes stops? The second line of the above examples shows that it does not stop until there is no more expression to take (the expression here could also be written `-(1 +1 +1)`).

This could be controlled by writing expressions like `(-1 +1) +1`, but there is a more sugary syntax for this: `-1 +1. +1`. The `.` here makes the parser terminate the expression it is parsing, and this makes `1 +1` the argument of the `-` unary operator. The expression required by the print statement is not over though (the argument of `-` is a sub expression) and its parsing continues to include the last `+1`.

`-1.` is not to be thought as some kind of negative floating point literal. There is no such thing in SFLK.

#### Some other stuff

##### How to include other scripts

```sflk
dh fi "file.sflk"
```

`fi` is a unary operator that takes a file path as its argument and evaluates into the content of the said file as a string. `dh` is the keyword of the do-here statement (which is like the do statement, but instead of executing code in a new context, code is executed in the current context, here). This piece of code executes the content of the given file in the current context.

##### What are contexts

(*TODO: Some explainations here are redundent with the "Signals" section. This redundency should be factored out.*)

A context is a place where variables are defined. When doing `x! < 8`, then a new variable named `x` is defined in the current context (if it was not already defined in that context). A simple assignment statement like `x < 8` does not define `x` (note the missing `!`), it will just assign `8` to an already defined `x` variable. Which one ?

Constexts are arranged as a tree. The context in which the execution takes place (the current context) is always a leaf. `x < 8` will search for an `x` variable starting from the current context and going toward the root (ignoring the other branches) and will write `8` to the first it finds (if any). The same logic applies to reading values from variables, a statement like `pr x + y` will search an `x` variable to read a value from, and then the same goes for `y`, in a manner similar to a search performed by an assignment.

```sflk
x! < "uwu"
pr x nl # A #
do {
	pr x nl # B #
	x! < "owo"
	pr x nl # C #
}
pr x nl # D #
```

In this example, the print statement A prints `uwu` as it reads the variable `x` of the current context. Then, the do statement creates a new context that is a child (in the context tree) of the current context, then this child becomes the current context for the execution of the code block given to the do statement. Then, the print statement B is executed, and this time there is no `x` variable in the current context, but as said earlier, this is not a problem. The parent context has an `x` variable, so it is read, and the print statement B prints `uwu` like A. Then, the print statement C prints `owo` as now there is an `x` variable in the current context. Then, the code block executed by the do statement comes to an end, and that brings the second context (in which `x` has the value `"owo"`) to be discarded, the first context (in which `x` has the value `"uwu"`) becomes the current context again. Then, the print statement D prints `uwu` because we are back in the first context where the `x` variable is untouched.

Should the `x! < "owo"` statement in the do statement block be replaced with `x < "owo"`, it would mean something else entierly. No `!` means that we are not defining a new variable, we are just assigning to an existing `x` variable, and here that would be the one that was initialized to `"uwu"`. So the print statement D would print `owo` instead of `uwu`.

##### Lists

```sflk
ev (), 3,  8, 18
ev     3,, 8, 18
```

The `()` literal evaluates to an object called nothing (every language has one of those). The `,` operator appends its operand to the list given on its left, and `()` acts as an empty list for `,`, so the first expression evaluates into a list containing `3`, `8` and `18`.

The `,,` operator is sugar that makes `a,, b` evaluate like `(), a, b` would.

```sflk
x < "a",, "us", {pr "mog"}
pr x ix 0 do x ix 2 pr x ix 1 nl
```

The `ix` binary operator allow access to a list element via its 0-based index. The above example also demonstrates that lists are not bound to contain only one type (this is a dynamic language after all, all the footguns of dynamic languages are to be supported by SFLK, it would be no fun otherwise).

##### Nop

```sflk
np
```

Does nothing on a Sunday morning. Also does nothing any other day.

##### If then else

```sflk
if x
th pr "then"
el pr "else"
```

Here goes your if-then-else statement. Except SFLK wants to be special, so then and else branches are optional, there can be multiples of them, and they can be given in any order and even interleaved. The condition `x` is required though.

##### Loop

```sflk
lp
wh x
bd dh {pr x x < x-1}
sp pr ", "
```

Loop, while `x`, execute the body (`bd`) statement. In between executions of the body, execute the separator (`sp`) statement.
Similar to the if statement, all these extensions are optional and can be given in any quantity and in any order.

The separator extension to loop statements are imho a very cool feature that more languages should have. It does not cost a lot in terms of language design but allows removing a code duplication pattern that occurs in pieces of code such as the one featured in the above example. Usually the `pr x` part of the code is duplicated (one instance in the loop body, and one instance before or after the loop) to account for the fact that *n* elements are printed but only *n-1* commas, but the loop body runs either *n-1* or *n* times. Granted, duplication could be avoided in other ways, but still.

##### Signals

(*TODO: Some explainations here are redundent with the "What are contexts" section. This redundency should be factored out.*)

```sflk
pr "h" nl
```

This actually does not *just* print stuff. Printing to the console is a side effect. Such side effects are frowned upon in some religious comunities. SFLK empowers the programmer to have complete control over these thanks to the following mechanism:

`pr "h"` actually sends a signal that travels through the context tree toward the root, and finally exits the isolated bubble where the execution takes place to reach the interpreter and ask for an interaction with the rest of the universe (here, the console). The interpreter then performs the required action, and potentially returns a result (but not in the case of a print statement, as there is nothing to answer from it). The execution then continues.

Wtf? Indeed, here are more details: When something like a do statement is executed, the new context that is created to run a piece of code in is a sub-context of the current context. Thus all the contexts that exist at a given time are organized in a tree. If the do statement registers an interceptor with the `wi` (With Interceptor) extention, then the interceptor will then intercept all the signals that come from sub-contexts as they travel towards the root. The interceptor may examine the signal, let it pass, send another signal, discard it, whatever.

There is no side effect that a context can do without all its parent contexts agreeing to it.

The details of how this works are still pretty unstable, but to give an idea, here is an example:

```sflk
do {
	pr "life in yellow~" nl
} wi {
	cy v
	if name - "print"
	el pr "\e[33m" + value + "\e[39m"
	th em v rs v
}
```

The with-interceptor extention (`wi`) takes a code block that is registered in the sub-context as the interceptor. Intercepted signals are available for inspection in the `v` variable. The final value of the variable `v` will be what the intercepted signal returns.

In the above example, only the print statements are tinkered with, and the other signals are handled by the statement `em v rs v` which re-emits them and forwards their result, so that everything happens as it they were not intercepted.

Another use case for this feature:

```sflk
do fi "something.sflk"
wi {
	cy v
	if name - "input"
	el v < "Morbius"
	th em v rs v
}
```

Here, we suppose that the file `something.sflk` contains an SFLK script that may at some point ask the user about its favorite movie (via the input expression `in` that evaluates into what the user types in the console). But here for some reason you don't want to have to type the name of your favorite movie every time you run the script (but you also don't want to modify the script). So you can write another script that executes `something.sflk` and makes the subscript behave as if you typed `Morbius` in response to every request, even though you are never requested to type anything anymore.

Both these examples are pretty dumb but imho this is a very nice feature with truly useful use cases!

##### Coming soon

Coming soon!

### Binary operator behaviors

#### Plus `+`

| left type  | right type | behavior
|:----------:|:----------:| --------
| integer    | integer    | Arithmetic addition
| string     | string     | String concatenation
| code block | code block | Code block concatenation

#### Minus `-`

| left type  | right type | behavior
|:----------:|:----------:| --------
| integer    | integer    | Arithmetic subtraction
| string     | string     | String comparison (0 iff equal)

#### Star `*`

| left type  | right type | behavior
|:----------:|:----------:| --------
| integer    | integer    | Arithmetic multiplication
| string     | integer    | String repetition
| code block | integer    | Code block repetition

#### Slash `/`

| left type  | right type | behavior
|:----------:|:----------:| --------
| integer    | integer    | Arithmetic euclidean division
| string     | string     | Count non-overlapping occurrences of right in left

#### To right `>`

| left type  | right type | behavior
|:----------:|:----------:| --------
| any type   | code block | Execute right with left as `v`, evaluates to `v`
| integer    | list       | Same as `right ix left`

#### Comma `,`

| left type  | right type | result
|:----------:|:----------:| ------
| nothing    | any type   | A list with right in it
| list       | any type   | Left to which is appended right

#### Double comma `,,`

| left type  | right type | result
|:----------:|:----------:| ------
| any type   | any type   | A list with left and right in it

#### Index `ix`

| left type  | right type | result
|:----------:|:----------:| ------
| list       | integer    | The right-th element of left
| string     | integer    | The right-th character of string

### Unary operators behaviors

#### Unary minus `-`

| right type | behavior
|:----------:| --------
| integer    | Arithmetic negation

#### File `fi`

| right type | behavior
|:----------:| --------
| string     | Read file at path right

#### Ordered `od`

| right type | result
|:----------:| ------
| list       | if the list is ordered then `1` else `0`

#### Ordered but strictly `os`

| right type | result
|:----------:| ------
| list       | if the list is strictly ordered then `1` else `0`

#### Length `ln`

| right type | result
|:----------:| ------
| list       | number of elements
| string     | number of characters

## Contribute

If you want to contribute in any way, please feel free to do so ^^.

Note that there is a Discord server dedicated to the *development and use* of SFLK (how to get in there? we don't know haha, maybe ask for an invite in one way or another).

## FAQ

### What does SFLK means?

Nothing haha. Similarly to LLVM, it is an acronym-like name without more meaning.

### What is it that this language tries to achieve?

Well... You know...

Ok, to be honest here, I have no idea where this is going. Designing a programming language is a common exercise that programmers are supposed to be capable of at some point, and it is plenty fun! However, it takes time, and life is short enough already, so I don't want to spend months to implement a clone of a successful language that will differentiate itself by just being less good than the original. I want to have fun programming in SFLK! This is all that matters, this is the only SFLK development principle.

This is not an [esolang](https://esolangs.org), but it is not a serious C/Python-wannabe that has "Safe, blazing fast🚀 system programming language" as its description. There are thousands of new languages like this every month, most of which never really make it. Instead, this is an unsafe, slow scripting programming language that hopes to provide an interesting programming experience to people who are lost in life and end up trying it.

### Implemented in Rust huh?

Rust blah blah you know already.

### Is there a roadmap?

Mmm, there is a [TODO](TODO.md) list, which is better than nothing.

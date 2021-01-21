
# SFLK programming language

```sflk
pr  3  - (1  +  1) # prints 1 #
pr (3  -  1) +  1  # prints 3 #
pr  3  -  1  +  1  # prints 3 #
```

```sflk
uwu < "o"
uwu < uwu + "wo"
pr uwu # prints owo #
```

```sflk
f < {
    x < 2
    pr x + 2 # prints 4 #
}
do f
do {
    pr "uwu" # prints uwu #
}
```

```sflk
pr "u" + ("wu" * 3) # prints uwuwuwu #
pr "lalala" / "la"  # prints 3 #
pr (100/10) - (2*5) # prints 0 #
pr  100/10  -  2*5  # prints 40 #
# operations are computed from left to right #
```

```sflk
# counts from 9 down to 1 #
x <~ 9 # assign only if x is free #
pr x
x < x -1
if x redo 0 # 0 is the current execution context #
```

```sflk
x <~ 4 do {
    imp 1 # import variables from 1 context below #
        pr x
        x < x -1
    exp 1 # export variables to 1 context below #
} if x redo 0
# prints numbers from 4 down to 1 #
```

It has
assignment, `+`, `-`, strings, integers, print, parenthesis, comments,
code blocks, `*`, `/`, assignment if free, loops, .
And more is comming soon!

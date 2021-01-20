
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
x <~ 9
pr x
x < x -1
if x redo
```

It has
assignment, `+`, `-`, strings, integers, print, parenthesis, comments,
code blocks, `*`, `/`.
And more is comming soon!

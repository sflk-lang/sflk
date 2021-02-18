<img src="/logo/sflk-logo-color.svg" align="right" width="300" alt="SFLK logo" />

# SFLK programming language

The language is very unstable as it is still in very early development.
The examples provided here may not be accurate.

```sflk
uwu < "o"
uwu < uwu + "wo"
pr uwu # prints owo # nl
```

```sflk
pr  3  - (1  +  1) # prints 1 # nl
pr (3  -  1) +  1  # prints 3 # nl
pr  3  -  1  +  1  # prints 3 # nl
# default is ((( ) ) ) #
```

```sflk
f < {
    x < 2
    pr x + 2 # prints 4 # nl
}
do f
do { pr "uwu" # prints uwu # nl }
```

```sflk
pr "u" + ("wu" * 3) # prints uwuwuwu # nl
pr "lalala" / "la"  # prints 3 # nl
pr (100/10) - (2*5) # prints 0 # nl
pr  100/10  -  2*5  # prints 40 # nl
# operations are computed from left to right #
```

```sflk
x <~ 9 # assign only if x is free #
pr x pr " "
x < x -1
if x redo 0 # 0 is the current execution context #
nl
# prints 9 8 7 6 5 4 3 2 1 #
```

```sflk
x <~ 4 do {
    imp 1 # import variables from 1 context below #
        pr x pr " "
        x < x -1
    exp 1 # export variables to 1 context below #
} if x redo 0
# prints numbers from 4 down to 1 #
```

```sflk
f < { pr "uwu" pr " " }
g < { pr "owo" nl }
do f + g # prints what you want to belive it prints ^^ #
```

```sflk
n < 9
b < { imp 1 x <~ n pr x x < x -1 exp 1 }
do b * n # prints 987654321 # nl
```

```sflk
x < 1
if x (pr "uwu " pr x nl) # prints uwu 1 #
f < {pr v *10 nl}
ev 8 >{pr v nl} -4 >f # print 8 and then 40 #
```

It has assignments, `+`, `-`, strings, integers, prints, parenthesis, comments,
first class code blocks, `*`, `/`, assignments if free, loops, nops.
And more is comming soon!

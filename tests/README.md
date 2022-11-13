# SFLK tests

Files in "tests/" are SFLK files that use various features of SFLK.
These are not Rust tests and currently there is no way to tell if the tests
are passing except by examining the output to see if it is the expected output.

## Run a test

For example run `test4.sflk`:

```sh
cargo run -- tests/test4.sflk
```

Adding `-d` to the arguments make the interpreter enable debug mode.

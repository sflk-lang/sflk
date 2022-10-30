#!/bin/sh

# Use the `tree` command to print a representation of the
# project file structure (omitting irrelevant directories).

tree --dirsfirst -a -I "target|local|.git|.vscode" --noreport

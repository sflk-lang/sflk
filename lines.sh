#!/bin/sh

# Use the `wc` command to count the lines of code of the
# SFLK interpreter. This serves no purpose but it feels
# good sometimes to remember that I can work on a passion
# project without dropping it 3 weeks later for an other one.
# This is a superficial yet satisfying statistic, and it
# reveals how superficial I am, experiencing joy from
# the subjectively high number of lines of code I wrote.

# Come on, print it. Again. Again! Harder! Come on!
# How many lines again? Huh? That many!? Oahh! So much code!

cat sflk-lang/src/*.rs | wc -l

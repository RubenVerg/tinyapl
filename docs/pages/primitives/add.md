---
glyph: '+'
pattern: 'r←x+y'
name: Add
---

`r` is the result of the scalar application of the addition function:

* if `x` and `y` are numbers, `r` is $x+y$
* if one of `x` and `y` is a character, and the other is an integer, `r` is the character with Unicode codepoint equal to the sum of the codepoint of the character argument plus the integer argument
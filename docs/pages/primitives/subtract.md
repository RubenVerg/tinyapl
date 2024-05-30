---
glyph: '-'
pattern: 'r←x-y'
name: Subtract
---

`r` is the result of the scalar application of the subtraction function:

* if `x` and `y` are numbers, `r` is $x - y$
* if `x` is a character and `y` is an integer, `r` is the character with Unicode codepoint equal to the codepoint of `x` minus `y`
* if `x` and `y` are characters, `r` is the difference between the Unicode codepoints of `x` and `y`
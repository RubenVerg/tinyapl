---
glyph: '='
pattern: 'r←x=y'
name: Equal To
---

`r` is the result of the scalar equality comparison of `x` and `y`:

* if `x` and `y` are both characters, `r` is true if they are the same character;
* if `x` and `y` are both numbers, `r` is true if `|x-y` is less than or equal to `ct×x⌈⍥|y`, where `ct` is the constant for comparison tolerance which is set to `1⏨¯14` ($10^{-14}$);
* if `x` and `y` are of different types, `r` is always false.

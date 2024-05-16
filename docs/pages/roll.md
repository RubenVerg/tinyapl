---
glyph: '?'
pattern: 'r←?y'
name: Roll
planned: true
---

`r` is the result of the scalar application of the roll function on `y`, which must be a natural array:

* if `y` is `0`, `r` is a random float between `0` and `1`, exclusive
* otherwise, `r` is a random integer from `1` to `y`, inclusive.
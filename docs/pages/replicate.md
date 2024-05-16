---
glyph: '⌿'
pattern: 'r←x⌿y'
name: Replicate
---

`x` is a vector of naturals with length equal to the tally of `y`. `r` is an array containing major cells `y` in the multiplicity indicated by the corresponding element of `x`. This means that cells that correspond to a `0` don't appear in `r`, cells that correspond to a `1` appear once, cells that correspond to a `2` appear twice, et cetera.
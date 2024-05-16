---
glyph: '§'
pattern: 'r←x§y'
name: Symmetric Difference
---

`r` is the symmetric difference of the major cells of `x` and `y`, that is, the cells of `x` that don't appear in `y` and the cells of `y` that don't appear in `x`: `r←(x~y)⍪y~x`.
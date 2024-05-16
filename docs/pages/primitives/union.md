---
glyph: '∪'
pattern: 'r←x∪y'
name: Union
---

`r` is the union of the major cells of `x` and `y`, i.e. all cells of `x` followed by all cells of `y` not in `x`: `r←x⍪y⌿⍨~y∊x`.
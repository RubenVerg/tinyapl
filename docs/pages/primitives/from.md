---
glyph: '⊇'
pattern: 'r←x⊇y'
name: From
planned: true
---

`x` must be a either a simple array of integers or a array of nested simple arrays of integers.

If `x` is simple, `r` is the major cells of `y` selected by the indices `x` (negative indices select from the end).

If `x` is nested, `r` is an array with the same shape as `x` where each entry is the element indexed by the corresponding entry of `x` in `y`.
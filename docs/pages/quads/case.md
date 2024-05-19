---
glyph: '⎕C'
pattern: 'r←[x] ⎕C y'
name: Case
category: Core
planned: true
---

`x` must be one of the scalar integers `1`, `0`, `¯1`, and defaults to `0`. `y` must be a character array.

If `x` is `1`, `r` is `y` with each character uppercased; if `x` is `0`, `r` is `y` with each character case-folded (for caseless comparison); if `x` is `¯1`, `r` is `y` with each character lowercased.
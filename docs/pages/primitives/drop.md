---
glyph: '↓'
pattern: 'r←x↓y'
name: Drop
---

`x` must be a scalar or vector of integers. `r` is the result of removing from each axis of `y` as many elements as specified by the corresponding element of `x`. If `x` is positive, the first `x` elements are removed; if `x` is negative, the last `x` elements are removed; if `x` is zero, no element is removed.
---
glyph: '⊖'
pattern: 'r←x⊖y'
name: Rotate
---

`x` must be a scalar or vector of integers. `r` is the result of rotating each axis of `y` by the amount specified by the corresponding item of `x`. If `x` is positive, the first `x` elements are moved to the back; if `x` is negative, the last `x` elements are moved to the front; if `x` is zero, no rotation is performed.
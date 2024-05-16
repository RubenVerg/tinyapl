---
glyph: '⊥'
pattern: 'r←[x]⊥y'
name: Decode
planned: true
---

If `x` is not provided, it defaults to the scalar `2`. If `x` is a scalar, it is reshaped to the shape of `y`. Operates on rank 1. The list `y` is interpreted in the mixed base defined by `x`, which is read as a list of weights: each value of `y` is multiplied with the corresponding value from `×⇞1↓x⍪1` and summed.
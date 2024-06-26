---
glyph: '⌊'
pattern: 'r←⌊y'
name: Floor
---

`r` is the result of the scalar application of the floor function, $\left\lfloor y \right\rfloor$.

The complex floor is defined [by McDonnell](https://www.jsoftware.com/papers/eem/complexfloor.htm) as such:

* let `a` and `b` be the real and imaginary parts of `y`
* let `x` be `1|a` and `y` be `1|b`
* let `p` be `a⊕⍥⌊b`, where the floor is the usual real floor
* if `x + y` is less than `1`, `r` is `p`
* if `x` is greater than or equal to `y`, `r` is `p+1`
* otherwise, `r` is `p+0ᴊ1`
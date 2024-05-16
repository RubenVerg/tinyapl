---
glyph: '≡'
pattern: 'r←≡y'
name: Depth
planned: true
---

`r` is a natural describing the *depth* of `y`:

* if `y` is a simple scalar (a number or character), `r` is `0`
* if `y` is an array and all its elements are simple scalars, `r` is `1`
* otherwise, `r` is one more than the maximum depth of each of the elements of `y`.
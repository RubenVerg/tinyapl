---
glyph: '⊤'
pattern: 'r←[x]⊤y'
name: Encode
planned: true
---

If `x` is not provided, it defaults to the scalar `2`.

If `x` is non-scalar, operates on vectors of `x` and scalars of `y`: the last item of `r` is the remainder of `y` divided by the last element of `x`; the second-to-last item of `r` is the remainder of the integer division of `y` and the last element of `x` divided by the second-to-last element of `x`, and so on. If an item of `x` is `0`, that item in `r` is set to the remainder value being processed and all elements of `r` before it are `0`.

If `x` is scalar, works as `⊤` were called with a left argument of an infinite list of `x`s, and all trailing `0`s are dropped.
---
glyph: 'ϼ'
pattern: 'r←xϼy'
name: Rerank
planned: true
---

`x` must be a natural. `r` is the result of changing the rank of `y` to be `x`:

* if the rank of `y` is `x`, `r` is `y`
* if the rank of `y` is greater than `x`, the leading axes of `y` are combined until the rank is `x`
* if the rank of `y` is less than `x`, leading axes of length `1` are introduced until the rank is `x`.
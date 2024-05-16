---
glyph: '@'
pattern: 'r←[x]({Fn}@{Gm})y'
name: At
planned: true
---

If `x` is provided, the left operand must be a function.

`r` is the result of substituting elements indicated by `G` or `m` with elements provided by `F` or `n`: if `m` is provided, it is a list of indices of major cells of `y` to be substituted; if `G` is provided, it is a function that returns a mask of the same shape as `y`; if `n` is provided, it is an array whose items will be used to replace the selected ones; if `F` is provided, it is a function that is applied to the selected items (as a whole), optionally with left argument `x`.
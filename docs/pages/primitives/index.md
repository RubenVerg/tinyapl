---
glyph: '⌷'
pattern: 'r←x⌷y'
name: Index
planned: true
---

`x` is a vector of integers with length less than or equal to the rank of `y`, and depth at most two. `r` is the selection of cells from axes of `y`, where nested entries of `x` indicate that multiple cells are selected, and negative `x` indicates indexing from the end.
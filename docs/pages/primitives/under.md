---
glyph: '⍢'
pattern: 'r←[x](F⍢G)y'
name: Under
---

`G` is a structural monadic function, i.e. a function that only reorders the argument without changing the elements' values. `r` is the result of applying `G`, calling \[`x∘`\]`F` on the reordered elements and then undo the reordering.
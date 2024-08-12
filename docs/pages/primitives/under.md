---
glyph: '⍢'
pattern: 'r←[x]({Fn}⍢G)y'
name: Under
---

`G` is a structural monadic function, i.e. a function that only reorders the argument without changing the elements' values. `r` is the result of applying `G`, calling \[`x∘`\]`F`, or `n⍨` on the reordered elements and then undo the reordering. If the result of `F`/`n⍨` is scalar, the result is repeated for all substituted elements of the result.
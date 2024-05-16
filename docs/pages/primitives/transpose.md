---
glyph: '⍉'
pattern: 'r←[x]⍉y'
name: Transpose
planned: true
---

`x` must be a vector of naturals, and each item must be at most the rank of `y`. If `x` is not provided, it is set to `⊖⍳ϼy`.

If `x` contains no duplicates, it must be a permutation of `⍳ϼy`. `r` is the permutation of the elements of `y` such that for each index `i`, `i⌷x⍉y` is `(x⊇i)⌷y`.

If `x` contains duplicates, the duplicate axes only collect entries where the indices match, effectively selecting diagonals.
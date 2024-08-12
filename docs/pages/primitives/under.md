---
glyph: '⍢'
pattern: 'r←[x]({Fn}⍢G)y'
name: Under
---

`G` is a structural monadic function, i.e. a function that only reorders the argument without changing the elements' values. `r` is the result of applying `G`, calling \[`x∘`\]`F`, or `n⍨` on the reordered elements and then undo the reordering. If the result of `F`/`n⍨` is scalar, the result is repeated for all substituted elements of the result.

## Functions supported as `G`

* [`↑` Take](/docs/primitive/take)
* [`↓` Drop](/docs/primitive/drop)
* [`⊖` Reverse](/docs/primitive/reverse)
* [`⊖` Rotate](/docs/primitive/rotate)
* [`⍉` Transpose](/docs/primitive/transpose)
* [`⍴` Reshape](/docs/primitive/reshape)
* [`ϼ` Rerank](/docs/primitive/rerank)
* [`,` Ravel](/docs/primitive/ravel)
* [`∧` Promote](/docs/primitive/promote)
* [`∨` Demote](/docs/primitive/demote)
* [`⌷` Index](/docs/primitive/index)
* [`⊇` From](/docs/primitive/from)
* [`⊃` First](/docs/primitive/first)
* [`⊇` Last](/docs/primitive/last)
* [`⌿` Replicate](/docs/primitive/replicate)
* All combinations of these functions
* These function on any [`⍤` At Rank](/docs/primitive/at-rank)
* All dfns which are structural, as defined above

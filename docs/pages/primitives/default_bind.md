---
glyph: '⍛'
pattern: 'r←[x]({Fn}⍛{Gm})y'
name: Default Bind
---

Exactly one of the arguments is an array and exactly one of the arguments is a function. If `x` is provided, `r` is `x A y`, where `A` is the function operand. If `x` is not provided, if `n` is provided, `r` is `n G y`; if `m` is provided, `r` is `y F m`.
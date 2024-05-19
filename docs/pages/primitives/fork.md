---
glyph: '«»'
pattern: 'r←[x](F«G»H)y'
name: Fork
planned: true
---

Note that `«` and `»` are two separate operators, but only meant to be used together.

If `x` is not provided, `r` is `(F y) G (H y)`; if `x` is provided, `r` is `(x F y) G (x H y)`.

|Monad|Dyad|
|:----|:---|
|<img src="/combinators/fork_1.svg" width="128" alt="Fork (monad) diagram">|<img src="/combinators/fork_2.svg" width="128" alt="Fork (dyad) diagram">|
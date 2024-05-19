---
glyph: '⎕JSON'
pattern: 'r←[x] ⎕JSON y'
name: JSON
category: Core
planned: true
---

`x` must be a Boolean scalar and defaults to `0`.

If `x` is `0`, `y` must be a character vector and `r` is the result of parsing JSON code in `y` (objects are represented as nested arrays of pairs).

If `x` is `1`, `y` can be any array and `r` is the result of representing `y` as JSON, representing arrays of higher rank as nested lists.
---
glyph: '⎕Assert'
pattern: 'r←[x] ⎕Assert y'
name: Assert
category: Core
planned: true
---

`x` must be a character vector, and defaults to the empty vector.

If `0∊y`, fails with an assertion error with message `x`. Otherwise, `r` is `1`.
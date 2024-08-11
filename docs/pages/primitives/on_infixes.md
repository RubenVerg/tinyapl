---
glyph: '↟'
pattern: 'r←x(F↟)y'
name: On Infixed
---

`x` must be a scalar natural. `r` is the result of applying `F` to *infixes* of `y` of length `|x`:

* if `x` is positive, infixes are overlapping "windows";
* if `x` is negative, infixes are non-overlapping "chunks"; if `|x` does not divide evenly in the tally of `y`, the last chunk will be shorter than the others.
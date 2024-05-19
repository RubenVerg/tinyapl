---
glyph: '⎕Partition'
pattern: 'r←⎕Partition y'
name: Partition To Group
category: Core
planned: true
---

`y` is an integer vector representing a partition of an array as defined by the APL function Partition. `r` is the result of converting it to a representation that is suitable for [Group](/primitive/group): `r←+⤈2<⤈0⍪y`.
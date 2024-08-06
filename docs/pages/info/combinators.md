---
name: Combinators
---

A combinator is a function or operator that only refers to its arguments and operands without modifying them in any way.

|Symbol|APL expression|Bird[^1]|TinyAPL|Diagram|
|------|--------------|--------|-------|:------|
|$\mathrm I$|`y`|Identity|[`⊣`](/docs/primitive/left)/[`⊢`](/docs/primitive/right)|<img src="/combinators/same.svg" width="128" alt="Same diagram">|
|$\mathrm K$|`x`|Kestrel|[`⊣`](/docs/primitive/left)|<img src="/combinators/left.svg" width="128" alt="Left diagram">|
|$\kappa$|`y`|Kite|[`⊢`](/docs/primitive/right)|<img src="/combinators/right.svg" width="128" alt="Right diagram">|
|$\mathrm W$|`y F y`|Warbler|[`⍨`](/docs/primitive/duplicate)|<img src="/combinators/duplicate.svg" width="128" alt="Duplicate diagram">|
|$\mathrm C$|`y F x`|Cardinal|[`⍨`](/docs/primitive/commute)|<img src="/combinators/commute.svg" width="128" alt="Commute diagram">|
|$\mathrm B$|`F (G y)`|Bluebird|[`∘`](/docs/primitive/after)/[`⍤`](/docs/primitive/atop)/[`⍥`](/docs/primitive/over)|<img src="/combinators/compose.svg" width="128" alt="Compose diagram">|
|$\mathrm Q$|`G (F y)`|Queer|[`⍛`](/docs/primitive/before)|<img src="/combinators/reverse_compose.svg" width="128" alt="Reverse Compose diagram">|
|${\mathrm B}_1$|`F (x G y)`|Blackbird|[`⍤`](/docs/primitive/atop)|<img src="/combinators/atop.svg" width="128" alt="Atop diagram">|
|$\Psi$|`(G x) F (G y)`|Psi|[`⍥`](/docs/primitive/over)|<img src="/combinators/over.svg" width="128" alt="Over diagram">|
|$\mathrm S$|`y F (G y)`|Starling|[`⟜`](/docs/primitive/right_hook)/[`⇽`](/docs/primitive/right_fork)|<img src="/combinators/right_hook.svg" width="128" alt="Right Hook diagram">|
|$\Sigma$|`(F y) G y`|Violet Starling|[`⊸`](/docs/primitive/left_hook)/[`⇾`](/docs/primitive/left_fork)|<img src="/combinators/left_hook.svg" width="128" alt="Left Hook diagram">|
|$\mathrm D$|`x F (G y)`|Dove|[`∘`](/docs/primitive/after)/[`⟜`](/docs/primitive/right_hook)|<img src="/combinators/after.svg" width="128" alt="After diagram">|
|$\Delta$|`(F x) G y`|Zebra Dove|[`⍛`](/docs/primitive/before)/[`⊸`](/docs/primitive/left_hook)|<img src="/combinators/before.svg" width="128" alt="Before diagram">|
|$\Phi$|`(F y) G (H y)`|Phoenix|[`«»`](/docs/primitive/fork)|<img src="/combinators/fork_1.svg" width="128" alt="Fork (monad) diagram">|
|$\Phi_1$|`(x F y) G (x H y)`|Pheasant|[`«»`](/docs/primitive/fork)|<img src="/combinators/fork_2.svg" width="128" alt="Fork (dyad) diagram">|
|${\mathrm D}_2$|`(F x) G (H y)`|Dovekie|[`⊸`](/docs/primitive/left_hook) + [`⟜`](/docs/primitive/right_hook)|<img src="/combinators/bracket.svg" width="128" alt="Bracket diagram">|
|$\mathrm P$|`(y G x) F (x G y)`|Parrot[^2]|[`⸚`](/docs/primitive/mirror)|<img src="/combinators/mirror.svg" width="128" alt="Mirror diagram">|
|$\mathrm N$|`x F (x G y)`|Eastern Nicator|[`⇽`](/docs/primitive/right_fork)|<img src="/combinators/right_fork.svg" width="128" alt="Right Fork diagram">|
|$\mathrm \nu$|`(x F y) G y`|Western Nicator|[`⇾`](/docs/primitive/left_fork)|<img src="/combinators/left_fork.svg" width="128" alt="Left Fork diagram">|

Additionally, some other primitives have combinator-like behavior:

|APL expression|TinyAPL|Diagram|
|--------------|-------|:------|
|`n`|[`⍨`](/docs/primitive/constant)|<img src="/combinators/constant_1.svg" width="128" alt="Constant (monad) diagram">|
|`n`|[`⍨`](/docs/primitive/constant)|<img src="/combinators/constant_2.svg" width="128" alt="Constant (dyad) diagram">|
|`F y`|[`⁖`](/docs/primitive/valences)|<img src="/combinators/valences_1.svg" width="128" alt="Valences (monad) diagram">|
|`x G y`|[`⁖`](/docs/primitive/valences)|<img src="/combinators/valences_2.svg" width="128" alt="Valences (dyad) diagram">|


[^1]: Some combinators have bird names, originating from [To Mock a Mockingbird](https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird) by Raymond Smullyan. Some of the bird names are taken from the [Uiua combinator page](https://www.uiua.org/docs/combinators).
[^2]: I made this one up.
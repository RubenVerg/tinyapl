---
name: Combinators
---

A combinator is a function or operator that only refers to its arguments and operands without modifying them in any way.

|Symbol|APL expression|Bird[^1]|TinyAPL|Diagram|
|------|--------------|--------|-------|:------|
|$\mathrm I$|`y`|Identity|[`⊣`](/primitive/left)/[`⊢`](/primitive/right)|<img src="/combinators/same.svg" width="128" alt="Same diagram">|
|$\mathrm K$|`x`|Kestrel|[`⊣`](/primitive/left)|<img src="/combinators/left.svg" width="128" alt="Left diagram">|
|$\kappa$|`y`|Kite|[`⊢`](/primitive/right)|<img src="/combinators/right.svg" width="128" alt="Right diagram">|
|$\mathrm W$|`y F y`|Warbler|[`⍨`](/primitive/duplicate)|<img src="/combinators/duplicate.svg" width="128" alt="Duplicate diagram">|
|$\mathrm C$|`y F x`|Cardinal|[`⍨`](/primitive/commute)|<img src="/combinators/commute.svg" width="128" alt="Commute diagram">|
|$\mathrm B$|`F (G y)`|Bluebird|[`∘`](/primitive/after)/[`⍤`](/primitive/atop)/[`⍥`](/primitive/over)|<img src="/combinators/compose.svg" width="128" alt="Compose diagram">|
|$\mathrm Q$|`G (F y)`|Queer|[`⍛`](/primitive/before)|<img src="/combinators/reverse_compose.svg" width="128" alt="Reverse Compose diagram">|
|${\mathrm B}_1$|`F (x G y)`|Blackbird|[`⍤`](/primitive/atop)|<img src="/combinators/atop.svg" width="128" alt="Atop diagram">|
|$\Psi$|`(G x) F (G y)`|Psi|[`⍥`](/primitive/over)|<img src="/combinators/over.svg" width="128" alt="Over diagram">|
|$\mathrm S$|`y F (G y)`|Starling|[`⟜`](/primitive/right_hook)|<img src="/combinators/right_hook.svg" width="128" alt="Right Hook diagram">|
|$\Sigma$|`(F y) G y`|Violet Starling|[`⊸`](/primitive/left_hook)|<img src="/combinators/left_hook.svg" width="128" alt="Left Hook diagram">|
|$\mathrm D$|`x F (G y)`|Dove|[`∘`](/primitive/after)/[`⟜`](/primitive/right_hook)|<img src="/combinators/after.svg" width="128" alt="After diagram">|
|$\Delta$|`(F x) G y`|Zebra Dove|[`⍛`](/primitive/before)/[`⊸`](/primitive/left_hook)|<img src="/combinators/before.svg" width="128" alt="Before diagram">|
|$\Phi$|`(F y) G (H y)`|Phoenix|[`«»`](/primitive/fork)|<img src="/combinators/fork_1.svg" width="128" alt="Fork (monad) diagram">|
|$\Phi_1$|`(x F y) G (x H y)`|Pheasant|[`«»`](/primitive/fork)|<img src="/combinators/fork_2.svg" width="128" alt="Fork (dyad) diagram">|
|${\mathrm D}_2$|`(F x) G (H y)`|Dovekie|[`⊸`](/primitive/left_hook) + [`⟜`](/primitive/right_hook)|<img src="/combinators/bracket.svg" width="128" alt="Bracket diagram">|
|$\mathrm P$|`(y G x) F (x G y)`|Parrot[^2]|[`⸚`](/primitive/mirror)|<img src="/combinators/mirror.svg" width="128" alt="Mirror diagram">|

Additionally, some other primitives have combinator-like behavior:

|APL expression|TinyAPL|Diagram|
|--------------|-------|:------|
|`n`|[`⍨`](/primitive/constant)|<img src="/combinators/constant_1.svg" width="128" alt="Constant (monad) diagram">|
|`n`|[`⍨`](/primitive/constant)|<img src="/combinators/constant_2.svg" width="128" alt="Constant (dyad) diagram">|
|`F y`|[`⁖`](/primitive/valences)|<img src="/combinators/valences_1.svg" width="128" alt="Valences (monad) diagram">|
|`x G y`|[`⁖`](/primitive/valences)|<img src="/combinators/valences_2.svg" width="128" alt="Valences (dyad) diagram">|


[^1]: Some combinators have bird names, originating from [To Mock a Mockingbird](https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird) by Raymond Smullyan. Some of the bird names are taken from the [Uiua combinator page](https://www.uiua.org/docs/combinators).
[^2]: I made this one up.
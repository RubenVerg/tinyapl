---
name: Combinators
---

A combinator is a function or operator that only refers to its arguments and operands without modifying them in any way.

|Symbol|Lambda|Bird[^1]|TinyAPL|Diagram|
|------|------|--------|-------|:------|
|$\mathrm I$|$\lambda y.y$|Identity|[`⊣`](/primitive/left)/[`⊢`](/primitive/right)|<img src="/combinators/same.svg" width="128" alt="Same diagram">|
|$\mathrm K$|$\lambda xy.x$|Kestrel|[`⊣`](/primitive/left)|<img src="/combinators/left.svg" width="128" alt="Left diagram">|
|$\kappa$|$\lambda xy.y$|Kite|[`⊢`](/primitive/right)|<img src="/combinators/right.svg" width="128" alt="Right diagram">|
|$\mathrm W$|$\lambda Fy.Fyy$|Warbler|[`⍨`](/primitive/duplicate)|<img src="/combinators/duplicate.svg" width="128" alt="Duplicate diagram">|
|$\mathrm C$|$\lambda Fxy.Fyx$|Cardinal|[`⍨`](/primitive/commute)|<img src="/combinators/commute.svg" width="128" alt="Commute diagram">|
|$\mathrm B$|$\lambda FGy.F(Gy)$|Bluebird|[`∘`](/primitive/after)/[`⍤`](/primitive/atop)/[`⍥`](/primitive/over)|<img src="/combinators/compose.svg" width="128" alt="Compose diagram">|
|$\mathrm Q$|$\lambda FGy.G(Fy)$|Queer|[`⍛`](/primitive/before)|<img src="/combinators/reverse_compose.svg" width="128" alt="Reverse Compose diagram">|
|${\mathrm B}_1$|$\lambda FGxy.F(Gxy)$|Blackbird|[`⍤`](/primitive/atop)|<img src="/combinators/atop.svg" width="128" alt="Atop diagram">|
|$\Psi$|$\lambda FGxy.F(Gx)(Gy)$|Psi|[`⍥`](/primitive/over)|<img src="/combinators/over.svg" width="128" alt="Over diagram">|
|$\mathrm S$|$\lambda FGy.Fy(Gy)$|Starling|[`⟜`](/primitive/right_hook)|<img src="/combinators/right_hook.svg" width="128" alt="Right Hook diagram">|
|$\Sigma$|$\lambda FGy.G(Fy)y$|Violet Starling|[`⊸`](/primitive/left_hook)|<img src="/combinators/left_hook.svg" width="128" alt="Left Hook diagram">|
|$\mathrm D$|$\lambda FGxy.Fx(Gy)$|Dove|[`∘`](/primitive/after)/[`⟜`](/primitive/right_hook)|<img src="/combinators/after.svg" width="128" alt="After diagram">|
|$\Delta$|$\lambda FGxy.G(Fx)y$|Zebra Dove|[`⍛`](/primitive/before)/[`⊸`](/primitive/left_hook)|<img src="/combinators/before.svg" width="128" alt="Before diagram">|
|$\Phi$|$\lambda FGHy.F(Gy)(Hy)$|Phoenix|[`«»`](/primitive/fork)|<img src="/combinators/fork_1.svg" width="128" alt="Fork (monad) diagram">|
|$\Phi_1$|$\lambda FGHxy.F(Gxy)(Hxy)$|Pheasant|[`«»`](/primitive/fork)|<img src="/combinators/fork_2.svg" width="128" alt="Fork (dyad) diagram">|
|${\mathrm D}_2$|$\lambda FGHxy.G(Fx)(Hy)$|Dovekie|[`⊸`](/primitive/left_hook) + [`⟜`](/primitive/right_hook)|<img src="/combinators/bracket.svg" width="128" alt="Bracket diagram">|
|$\mathrm P$[^2]|$\lambda FGxy.F(Gyx)(Gxy)$|Parrot|[`⸚`](/primitive/mirror)|<img src="/combinators/mirror.svg" width="128" alt="Mirror diagram">|

Additionally, some other primitives have combinator-like behavior:

|Lambda|TinyAPL|Diagram|
|------|-------|:------|
|$\lambda nx.n$|[`⍨`](/primitive/constant)|<img src="/combinators/constant_1.svg" width="128" alt="Constant (monad) diagram">|
|$\lambda nxy.n$|[`⍨`](/primitive/constant)|<img src="/combinators/constant_2.svg" width="128" alt="Constant (dyad) diagram">|
|$\lambda FGy.Fy$|[`⁖`](/primitive/valences)|<img src="/combinators/valences_1.svg" width="128" alt="Valences (monad) diagram">|
|$\lambda FGxy.Gxy$|[`⁖`](/primitive/valences)|<img src="/combinators/valences_2.svg" width="128" alt="Valences (dyad) diagram">|


[^1]: Some combinators have bird names, originating from [To Mock a Mockingbird](https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird) by Raymond Smullyan. Some of the bird names are taken from the [Uiua combinator page](https://www.uiua.org/docs/combinators).
[^2]: I made this one up.
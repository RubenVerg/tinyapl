# TinyAPL

<center><img src="https://raw.githubusercontent.com/RubenVerg/tinyapl/main/logo.png" alt="TinyAPL logo"></center>

TinyAPL (read like *tiny apple*) is a tiny APL dialect and interpreter in Haskell. It is being implemented as a series of articles available [here](https://blog.rubenverg.com/?tag=tinyapl).

* Part 1: [Introduction & Arrays](https://blog.rubenverg.com/tinyapl_1_arrays)
* Part 2: [Functions & Operators](https://blog.rubenverg.com/tinyapl_2_functions_operators)
* Part 3: [More Primitives](https://blog.rubenverg.com/tinyapl_3_more_primitives)
* Part 4: [Finally, Parsing!](https://blog.rubenverg.com/tinyapl_4_parsing)

## Features

Marked features are supported, unmarked features are planned

* Types
	* [x] Arrays
		* [x] complex numbers
		* [x] unicode characters
		* [x] boxes (nested arrays)
	* [x] Functions
	* [x] Monadic Operators ("Adverbs")
	* [x] Dyadic Operators ("Conjunctions")
* Syntax
	* [x] number literals
		* [x] decimal point `.`
		* [x] negative sign `¯`
		* [x] exponent notation `⏨`
		* [x] complex notation `ᴊ`
		* [ ] complex phase notation? `∠`
		* [ ] infinities `∞`
	* [x] character literals `'`
	* [x] string literals `"`
		* [x] escapes with `⍘`
			* [x] `⍘⍘` -> `⍘`
			* [x] `⍘"` -> `"`
			* [x] `⍘n` -> newline
			* [x] `⍘r` -> carriage return
			* [x] `⍘t` -> tab
			* [ ] `⍘{22be}` -> unicode?
	* [x] names
		* [x] array names (`abc`)
		* [x] function names (`Abc`)
		* [x] monadic operator names (`_Abc`)
		* [x] dyadic operator names (`_Abc_`)
	* [x] assignment `←`
	* [x] dfns/dops
		* [x] multiple statements `⋄`
		* [x] guards `:`
		* [x] early return `■`
		* [x] dfns `{...}`, dadvs `_{...}`, dconjs `_{...}_`
		* [x] refer to arguments and operands: `⍺` left argument, `⍵` right argument, `⍺⍺` left array operand, `⍶⍶` left function operand, `⍵⍵` right array operand, `⍹⍹` right function operand
		* [x] recursion: `∇` recurse function, `_∇` recurse adverb, `_∇_` recurse conjunction
	* [ ] comments `⍝`
		* [ ] inline comments? `⟃`/`⟄`
	* [x] console I/O with `⎕` and `⍞`
* primitives
	* arrays
		* [x] `⍬`
	* number functions
		* [x] `+` conjugate
		* [x] `+` add
		* [x] `-` negate
		* [x] `-` subtract
		* [x] `×` direction
		* [x] `×` multiply
		* [x] `÷` reciprocal
		* [x] `÷` divide
			* `0÷0` is `1`
			* `x÷0` is an error
		* [x] `*` exponential
		* [x] `*` power
		* [x] `⍟` ln
		* [x] `⍟` log
			* `1⍟1` is `1`
			* `1⍟y` is an error
			* `x⍟0` is an error
		* [x] `○` pi times
		* [x] `○` circular
			* `0○y` is $\sqrt{1 - y^2}$
			* `1○y` is $\sin y$
			* `¯1○y` is $\arcsin y$
			* `2○y` is $\cos y$
			* `¯2○y` is  $\arccos y$
			* `3○y` is $\tan y$
			* `¯3○y` is $\arctan y$
			* `4○y` is $\sqrt{1 + y^2}$
			* `¯4○y` is $\sqrt{y^2 - 1}$
			* `5○y` is $\sinh y$
			* `¯5○y` is $\mathop{\text{arsinh}} y$
			* `6○y` is $\cosh y$
			* `¯6○y` is $\mathop{\text{arcosh}} y$
			* `7○y` is $\tanh y$
			* `¯7○y` is $\mathop{\text{artanh}} y$
			* `8○y` is $\sqrt{-1 - y^2}$
			* `¯8○y` is $-\sqrt{-1 - y^2}$
			* `9○y` is $\mathop{\text{Re}} y$
			* `¯9○y` is $y$
			* `10○y` is $\left|y\right|$
			* `¯10○y` is $\overline y$ (conjugate of $y$)
			* `11○y` is $\mathop{\text{Im}} y$
			* `¯11○y` is $iy$
			* `12○y` is $\mathop{\text{Arg}} y$
			* `¯12○y` is $e^{iy}$
		* [x] `√` square root
		* [x] `√` root
		* [x] `⌊` floor
		* [x] `⌊` minimum
		* [x] `⌈` ceiling
		* [x] `⌈` maximum
		* [x] `⸠` round
		* [ ] `⸠` average?
		* [ ] `⌹` matrix inverse
		* [ ] `⌹` matrix divide
		* [ ] `!` factorial/gamma
		* [ ] `!` binomial
		* [ ] `|` magnitude
		* [ ] `|` remainder
		* [ ] `∨` greatest common divisor
		* [ ] `∧` least common multiple
		* [x] `⊕` cartesian (`x⊕y` is $x + iy$)
		* [x] `⊕` pure imaginary (`⊕y` is $iy$)
		* [x] `⊗` polar (`x⊗y` is $xe^{iy}$)
		* [x] `⊗` unit polar (`⊗y` is $e^{iy}$)
		* [ ] `∡` phase?
		* [ ] `∡` set phase? (`(|y)⊗x`)
		* [ ] `⊥` decode
		* [ ] `⊥` base 2 decode
		* [ ] `⊤` encode
		* [ ] `⊤` base 2 encode
	* boolean functions
		* [x] `=` equals
		* [x] `≠` not equals
		* [x] `<` less
		* [x] `≤` less or equal
		* [x] `≥` greater or equal
		* [x] `>` greater
		* [x] `≡` match
		* [x] `≢` not match
		* [x] `∧` and
		* [x] `∨` or
		* [x] `⍲` nand
		* [x] `⍱` nor
		* [ ] `~` not
	* set functions
		* [ ] `∪` unique
		* [ ] `∪` union
		* [ ] `∩` intersection
		* [ ] `~` difference
		* [ ] `§` symmetric difference
		* [ ] `≠` nub sieve
		* [ ] multisets?
	* property functions
		* [ ] `⍳` index of
		* [ ] `⍸` interval index
		* [ ] `∊` element of
		* [ ] `⍷` find
		* [ ] `⍷` type? depends on if prototypes are added or not
		* [ ] `≡` depth
		* [x] `⍴` shape
		* [x] `≢` tally
		* [ ] `ϼ` rank
	* array creation functions
		* [x] `⍳` index generator
		* [ ] `?` roll
		* [ ] `?` deal
		* [ ] `…` range?
		* [x] `⍮` pair
		* [x] `⍮` half pair
		* [ ] `‿` link
	* array manipulation functions
		* [x] `↑` take
		* [x] `↓` drop
		* [ ] `↑` mix
		* [ ] `↓` split
		* [x] `⊂` enclose
		* [ ] `⊆` nest
		* [ ] `⊂` partitioned enclose
		* [ ] `⊆` partition
		* [ ] `⍋` grade up
		* [ ] `⍒` grade down
		* [ ] `≤` sort up
		* [ ] `≥` sort down
		* [x] `⍸` where
		* [ ] `∊` enlist
		* [ ] `⌿` replicate
		* [ ] `⍀` expand? (might not make much sense without 
		prototypes)
		* [x] `,` ravel
		* [ ] `⍪` table
		* [ ] `⍪` catenate
		* [x] `⍴` reshape
		* [ ] `ϼ` rerank (generalized version of promote/demote: introduce leading length-1 axes or combine leading axes)
		* [x] `⊖` reverse
		* [ ] `⊖` rotate
		* [ ] `⍉` transpose
		* [ ] `⍉` reorder axes
	* array lookup functions
		* [x] `⊃` first
		* [x] `⊇` last
		* [ ] `⊇` from
		* [ ] `⌷` index
		* [ ] `⊃` pick
	* misc functions
		* [x] `⊢`/`⊣` same
		* [x] `⊢` right
		* [x] `⊣` left
		* [ ] `⍎` execute
		* [ ] `⍕` format
		* [ ] `↗` raise?
	* last axis functions
		* [ ] `,` catenate last
		* [ ] `⌽` reverse last
		* [ ] `⌽` rotate last
	* operators
		* [ ] `/` reduce (first)
		* [ ] `/` windowed reduce (first)
		* [ ] `\` scan (first)
		* [ ] `¨` each
		* [ ] `ᐵ` each-left?
		* [ ] `ᑈ` each-right?
		* [ ] `⍣` repeat (with inverses?)
		* [ ] `⍣` until
		* [ ] `∙` inner product
		* [ ] `∙` alternant?
		* [ ] `⊞` outer product
		* [ ] `⍤` at rank
		* [ ] `⍥` at depth
		* [ ] `@` at
		* [ ] `⌸` key
		* [ ] `⌸` key with vocabulary
		* [ ] `⍁` diagonals?
		* [ ] `⌺` stencil
	* combinators
		* [x] `⍨` constant
		* [x] `⍨` commute
		* [x] `⍨` duplicate
		* [x] `∘` bind argument
		* [x] `∘` after
		* [x] `⍛` default argument (uses operand if called monadically, and argument if called dyadically)
		* [x] `⍛` before (not a hook!)
		* [x] `⊸` left hook
		* [x] `⟜` right hook
		* [x] `⍤` atop
		* [x] `⍥` over
		* [ ] `«`/`»` fork?
* system names (quad names)
	* [ ] support for system names in parsing and interpreting
	* system arrays (nilads, i guess)
		* [ ] `⎕a` (constant) the uppercase alphabet
		* [ ] `⎕l` (constant) the lowercase alphabet
		* [ ] `⎕d` (constant) the digits
		* [ ] `⎕ts` current Unix timestamp
		* [ ] `⎕io` (constant) `1`
		* [ ] `⎕ct` (constant) `1⏨¯14`
		* [ ] `⎕seed` (set only) seed the random number generator
	* system functions
		* [ ] `⎕File` read/write files
		* [ ] `⎕Time` convert between time formats
		* [ ] `⎕HTTP` http requests
		* [ ] `⎕CSV` convert from/to CSV
		* [ ] `⎕JSON` convert from/to JSON
		* [ ] `⎕Unicode` convert between unicode representations
	* system adverbs
		* [ ] `⎕_BinFile` read/write binary files, with format chosen from the operand
	* system conjunctions

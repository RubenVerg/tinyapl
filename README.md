# TinyAPL

<center><img src="https://raw.githubusercontent.com/RubenVerg/tinyapl/main/logo.png" width="100" alt="TinyAPL logo"></center>

TinyAPL (read like *tiny apple*) is a tiny APL dialect and interpreter in Haskell. It is being implemented as a series of articles available [here](https://blog.rubenverg.com/?tag=tinyapl).

* Part 1: [Introduction & Arrays](https://blog.rubenverg.com/tinyapl_1_arrays)
* Part 2: [Functions & Operators](https://blog.rubenverg.com/tinyapl_2_functions_operators)
* Part 3: [More Primitives](https://blog.rubenverg.com/tinyapl_3_more_primitives)
* Part 4: [Finally, Parsing!](https://blog.rubenverg.com/tinyapl_4_parsing)
* Part 5: [Array Notation and Reductions](https://blog.rubenverg.com/tinyapl_5_array_notation_reductions)
* Part 6: [Tests, Docs, Each](https://blog.rubenverg.com/tinyapl_6_tests_docs_each)
* Part 7: [Quads, Key, Index](https://blog.rubenverg.com/tinyapl_7_quads_key_index)
* Part 8: [All About Rank, and a Web Interface](https://blog.rubenverg.com/tinyapl_8_rank_web)
* Part 9: [More Tacit!](https://blog.rubenverg.com/tinyapl_9_more_tacit)

Documentation is available [here](https://tinyapl.rubenverg.com) and you can run the latest interpreter [here](https://tinyapl.rubenverg.com/run/latest)

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
		* [x] inline comments? `⟃`/`⟄`
	* [x] console I/O with `⎕` and `⍞`
	* [x] array notation `⟨`/`⋄`/`⟩` and `[`/`⋄`/`]`
	* [x] trains and modifier trains \[`_`\]`⦅`/`⋄`/`⦆`\[`_`\]
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
		* [x] `|` magnitude
		* [x] `|` remainder
		* [x] `∨` greatest common divisor
		* [x] `∧` least common multiple
		* [x] `⊕` cartesian (`x⊕y` is $x + iy$)
		* [x] `⊕` pure imaginary (`⊕y` is $iy$)
		* [x] `⊗` polar (`x⊗y` is $xe^{iy}$)
		* [x] `⊗` unit polar (`⊗y` is $e^{iy}$)
		* [x] `∡` phase
		* [ ] `∡` atan2
		* [x] `ℜ` real part
		* [x] `ℑ` imaginary part
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
		* [x] `≡` identical
		* [x] `≢` not identical
		* [ ] `⊲` precedes (TAO less)
		* [ ] `⊴` precedes or identical (TAO less or equal)
		* [ ] `⊵` succeeds or identical (TAO greater or equal)
		* [ ] `⊳` succeeds (TAO greater)
		* [x] `∧` and
		* [x] `∨` or
		* [x] `⍲` nand
		* [x] `⍱` nor
		* [x] `~` not
	* set functions
		* [x] `∪` unique
		* [x] `∪` union
		* [x] `∩` intersection
		* [x] `~` difference
		* [x] `§` symmetric difference
		* [x] `≠` nub sieve
		* [ ] multisets?
	* property functions
		* [ ] `⍳` index of
		* [ ] `⍸` interval index
		* [ ] `∊` element of
		* [ ] `⍷` find
		* [ ] `⍷` type? depends on if prototypes are added or not
		* [ ] `⋷` histogram (inverse of where)
		* [ ] `⋷` count
		* [x] `≡` depth
		* [x] `⍴` shape
		* [x] `≢` tally
		* [x] `ϼ` rank
	* array creation functions
		* [x] `⍳` index generator
		* [x] `?` roll
		* [ ] `?` deal
		* [ ] `…` range?
		* [x] `⍮` pair
		* [x] `⍮` half pair
	* array manipulation functions
		* [x] `↑` take
		* [x] `↓` drop
		* [ ] `↑` mix
		* [ ] `↓` major cells (split is `⊂⍤1`)
		* [x] `⊂` enclose
		* [ ] `⊆` nest
		* [ ] `⊂` partitioned enclose
		* [ ] `⊆` group (not partition!)
		* [ ] `⍋` grade up
		* [ ] `⍒` grade down
		* [ ] `⍋` sort by up?
		* [ ] `⍒` sort by down?
		* [ ] `≤` sort up? useless with sort by (`⍋⍨`)
		* [ ] `≥` sort down? as above
		* [x] `⍸` where
		* [x] `∊` enlist
		* [x] `⌿` replicate
		* [ ] `⍀` expand? (might not make much sense without prototypes)
		* [x] `,` ravel
		* [ ] `⍪` table
		* [ ] `⍪` catenate
		* [x] `⍴` reshape
		* [x] `ϼ` rerank (generalized version of promote/demote: introduce leading length-1 axes or combine leading axes)
		* [x] `⊖` reverse
		* [x] `⊖` rotate
		* [ ] `⍉` transpose
		* [ ] `⍉` reorder axes
		* [x] `∧` promote (introduce leading axis)
		* [x] `∨` demote (combine two leading axes)
		* [ ] `,` laminate
	* array lookup functions
		* [x] `⊃` first
		* [x] `⊇` last
		* [x] `⊇` from
		* [x] `⌷` index
		* [ ] `⊃` pick
	* misc functions
		* [x] `⊢`/`⊣` same
		* [x] `⊢` right
		* [x] `⊣` left
		* [ ] `⍎` execute
		* [ ] `⍕` format
		* [ ] `↗` raise?
		* [ ] `⇂` minimal (TAO minimum)
		* [ ] `↾` maximal (TAO maximum)
	* operators
		* [x] `⍆` reduce left-to-right
		* [ ] `⍆` windowed reduce left-to-right
		* [x] `⍅` reduce right-to-left
		* [ ] `⍅` windowed reduce right-to-left
		* [x] `↟` on prefixes
		* [x] `↡` on suffixes
		* [x] `¨` each
		* [x] `ᐵ` each-left
		* [x] `ᑈ` each-right
		* [x] `⍣` repeat
		* [x] `⍣` until
		* [ ] `∙` inner product
		* [ ] `∙` alternant?
		* [ ] `⊞` outer product
		* [x] `⍤` at rank
		* [ ] `⍥` at depth
		* [ ] `@` at
		* [x] `⌸` key
		* [ ] `⌸` key with vocabulary
		* [ ] `⍁` diagonals?
		* [ ] `⌺` stencil
		* [ ] `⁖` valences (call left if monad and right if dyad)
		* [ ] `⍢` strucutral under
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
		* [x] `⸚` mirror (`_{(⍵ ⍹⍹ ⍺)⍶⍶(⍺ ⍹⍹ ⍵)}_`)
		* [ ] `«`/`»` fork
		* [x] `⇾` left fork (`_{(⍺ ⍶⍶ ⍵) ⍹⍹ ⍵}_`)
		* [x] `⇽` right fork (`_{⍺ ⍶⍶ (⍺ ⍹⍹ ⍵)}_`)
* system names (quad names)
	* [x] support for system names in parsing and interpreting
	* system arrays (nilads, i guess)
		* [x] `⎕u` (constant) the uppercase alphabet
		* [x] `⎕l` (constant) the lowercase alphabet
		* [x] `⎕d` (constant) the digits
		* [ ] `⎕ts` current timestamp (year, month, day, hour, minute, second, millisecond)
		* [x] `⎕unix` current Unix timestamp
		* [x] `⎕io` (constant) `1`
		* [x] `⎕ct` (constant) `1⏨¯14`
		* [x] `⎕seed` (set only) seed the random number generator
	* system functions
		* [ ] `⎕File` read/write files
		* [ ] `⎕DateTime` (or `⎕DT` maybe?) convert between time formats
		* [ ] `⎕HTTP` http requests
		* [ ] `⎕CSV` convert from/to CSV
		* [ ] `⎕JSON` convert from/to JSON
		* [ ] `⎕Unicode` convert between unicode representations
		* [x] `⎕Exists` does a variable exist?
		* [ ] `⎕C`/`⎕Case` case fold/uppercase/lowercase
		* [ ] `⎕Partition` convert Partition representation to Group representation (so that Partition is `⎕Partition⍛⊆`)
		* [x] `⎕Repr` convert an array to a representation that can be read with Execute
		* [ ] `⎕Assert` assertions
		* [x] `⎕Delay` sleep for n seconds
	* system adverbs
		* [ ] `⎕_BinFile` read/write binary files, with format chosen from the operand
	* system conjunctions

module TinyAPL.Glyphs where

-- * Syntax

decimal = '.'
negative = '¯'
exponent = '⏨'
imaginary = 'ᴊ'
infinity = '∞'
charDelimiter = '\''
stringDelimiter = '"'
stringEscape = '⍘'
parens = ('(', ')')
braces = ('{', '}')
vector = ('⟨', '⟩')
highRank = ('[', ']')
underscore = '_'
separator = '⋄'
assign = '←'
guard = ':'
exit = '■'
inlineComment = ('⟃', '⟄')

syntax =
  [ TinyAPL.Glyphs.decimal
  , TinyAPL.Glyphs.negative
  , TinyAPL.Glyphs.exponent
  , TinyAPL.Glyphs.imaginary
  , TinyAPL.Glyphs.infinity
  , TinyAPL.Glyphs.charDelimiter
  , TinyAPL.Glyphs.stringDelimiter
  , TinyAPL.Glyphs.stringEscape
  , fst TinyAPL.Glyphs.parens
  , snd TinyAPL.Glyphs.parens
  , fst TinyAPL.Glyphs.braces
  , snd TinyAPL.Glyphs.braces
  , fst TinyAPL.Glyphs.vector
  , snd TinyAPL.Glyphs.vector
  , fst TinyAPL.Glyphs.highRank
  , snd TinyAPL.Glyphs.highRank
  , TinyAPL.Glyphs.underscore
  , TinyAPL.Glyphs.separator
  , TinyAPL.Glyphs.assign
  , TinyAPL.Glyphs.guard
  , TinyAPL.Glyphs.exit
  , fst TinyAPL.Glyphs.inlineComment
  , snd TinyAPL.Glyphs.inlineComment ]

escapes =
  [ (stringDelimiter, stringDelimiter)
  , (charDelimiter, charDelimiter)
  , (stringEscape, stringEscape)
  , ('n', '\n')
  , ('r', '\r')
  , ('t', '\t') ]

-- * Identifiers

delta = '∆'
deltaBar = '⍙'
alpha = '⍺'
alphaBar = '⍶'
omega = '⍵'
omegaBar = '⍹'
quad = '⎕'
quadQuote = '⍞'
del = '∇'

identifiers =
  [ TinyAPL.Glyphs.delta
  , TinyAPL.Glyphs.deltaBar
  , TinyAPL.Glyphs.alpha
  , TinyAPL.Glyphs.alphaBar
  , TinyAPL.Glyphs.omega
  , TinyAPL.Glyphs.omegaBar
  , TinyAPL.Glyphs.quad
  , TinyAPL.Glyphs.quadQuote
  , TinyAPL.Glyphs.del ]

-- * Arrays

zilde = '⍬'

arrays =
  [ TinyAPL.Glyphs.zilde ]

-- * Functions

plus = '+'
minus = '-'
times = '×'
divide = '÷'
power = '*'
logarithm = '⍟'
circle = '○'
root = '√'
floor = '⌊'
ceil = '⌈'
round = '⸠'
less = '<'
lessEqual = '≤'
equal = '='
greaterEqual = '≥'
greater = '>'
notEqual = '≠'
and = '∧'
or = '∨'
nand = '⍲'
nor = '⍱'
cartesian = '⊕'
polar = '⊗'
identical = '≡'
notIdentical = '≢'
rho = '⍴'
ravel = ','
reverse = '⊖'
pair = '⍮'
enclose = '⊂'
first = '⊃'
last = '⊇'
take = '↑'
drop = '↓'
left = '⊣'
right = '⊢'
iota = '⍳'
indices = '⍸'
replicate = '⌿'
abs = '|'
phase = '∡'
real = 'ℜ'
imag = 'ℑ'
union = '∪'
intersection = '∩'
difference = '~'
symdiff = '§'
element = '∊'
roll = '?'
squad = '⌷'

functions =
  [ TinyAPL.Glyphs.plus
  , TinyAPL.Glyphs.minus
  , TinyAPL.Glyphs.times
  , TinyAPL.Glyphs.divide
  , TinyAPL.Glyphs.power
  , TinyAPL.Glyphs.logarithm
  , TinyAPL.Glyphs.circle
  , TinyAPL.Glyphs.root
  , TinyAPL.Glyphs.floor
  , TinyAPL.Glyphs.ceil
  , TinyAPL.Glyphs.round
  , TinyAPL.Glyphs.less
  , TinyAPL.Glyphs.lessEqual
  , TinyAPL.Glyphs.equal
  , TinyAPL.Glyphs.greaterEqual
  , TinyAPL.Glyphs.greater
  , TinyAPL.Glyphs.notEqual
  , TinyAPL.Glyphs.and
  , TinyAPL.Glyphs.or
  , TinyAPL.Glyphs.nand
  , TinyAPL.Glyphs.nor
  , TinyAPL.Glyphs.cartesian
  , TinyAPL.Glyphs.polar
  , TinyAPL.Glyphs.identical
  , TinyAPL.Glyphs.notIdentical
  , TinyAPL.Glyphs.rho
  , TinyAPL.Glyphs.ravel
  , TinyAPL.Glyphs.reverse
  , TinyAPL.Glyphs.pair
  , TinyAPL.Glyphs.enclose
  , TinyAPL.Glyphs.first
  , TinyAPL.Glyphs.last
  , TinyAPL.Glyphs.take
  , TinyAPL.Glyphs.drop
  , TinyAPL.Glyphs.left
  , TinyAPL.Glyphs.right
  , TinyAPL.Glyphs.iota
  , TinyAPL.Glyphs.indices
  , TinyAPL.Glyphs.replicate
  , TinyAPL.Glyphs.abs
  , TinyAPL.Glyphs.phase
  , TinyAPL.Glyphs.real
  , TinyAPL.Glyphs.imag
  , TinyAPL.Glyphs.union
  , TinyAPL.Glyphs.intersection
  , TinyAPL.Glyphs.difference
  , TinyAPL.Glyphs.symdiff
  , TinyAPL.Glyphs.element
  , TinyAPL.Glyphs.roll
  , TinyAPL.Glyphs.squad ]

-- * Adverbs

selfie = '⍨'
reduce = '⍆'
reduceBack = '⍅'
onPrefixes = '↟'
onSuffixes = '↡'
each = '¨'
eachLeft = 'ᐵ'
eachRight = 'ᑈ'
key = '⌸'

adverbs =
  [ TinyAPL.Glyphs.selfie
  , TinyAPL.Glyphs.reduce
  , TinyAPL.Glyphs.reduceBack
  , TinyAPL.Glyphs.onPrefixes
  , TinyAPL.Glyphs.onSuffixes
  , TinyAPL.Glyphs.each
  , TinyAPL.Glyphs.eachLeft
  , TinyAPL.Glyphs.eachRight
  , TinyAPL.Glyphs.key ]

-- * Conjunctions

atop = '⍤'
over = '⍥'
after = '∘'
before = '⍛'
leftHook = '⊸'
rightHook = '⟜'

conjunctions =
  [ TinyAPL.Glyphs.atop
  , TinyAPL.Glyphs.over
  , TinyAPL.Glyphs.after
  , TinyAPL.Glyphs.before
  , TinyAPL.Glyphs.leftHook 
  , TinyAPL.Glyphs.rightHook ]

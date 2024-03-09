module TinyAPL.Glyphs where
import Data.Char (chr)

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
separator = '⋄'
assign = '←'
guard = ':'
exit = '■'

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
match = '≡'
notMatch = '≢'
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
  , TinyAPL.Glyphs.match
  , TinyAPL.Glyphs.notMatch
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
  , TinyAPL.Glyphs.indices ]

-- * Adverbs

selfie = '⍨'

adverbs =
  [ TinyAPL.Glyphs.selfie ]

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
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
  , TinyAPL.Glyphs.roll ]

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
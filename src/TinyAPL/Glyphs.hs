module TinyAPL.Glyphs where
import Data.Char (chr)

-- * Syntax

negative = chr 0xaf
exponent = chr 0x23e8
imaginary = chr 0x1d0a
infinity = chr 0x221e

-- * Functions

plus = '+'
minus = '-'
times = chr 0xd7
divide = chr 0xf7
power = '*'
logarithm = chr 0x235f
circle = chr 0x25cb
root = chr 0x221a
floor = chr 0x230a
ceil = chr 0x2308
less = '<'
lessEqual = chr 0x2264
equal = '='
greaterEqual = chr 0x2265
greater = '>'
notEqual = chr 0x2260
and = chr 0x2227
or = chr 0x2228
nand = chr 0x2372
nor = chr 0x2371
cartesian = chr 0x2295
polar = chr 0x2297
match = chr 0x2261
notMatch = chr 0x2262
rho = chr 0x2374
ravel = ','
reverse = chr 0x2296
pair = chr 0x236e
enclose = chr 0x2282
first = chr 0x2283
last = chr 0x2287
take = chr 0x2191
drop = chr 0x2193
left = chr 0x22a3
right = chr 0x22a2

-- * Adverbs

-- * Conjunctions

atop = chr 0x2364
over = chr 0x2365
after = chr 0x2218
before = chr 0x235b
leftHook = chr 0x22b8
rightHook = chr 0x27dc
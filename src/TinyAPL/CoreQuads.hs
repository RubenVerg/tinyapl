module TinyAPL.CoreQuads where

import TinyAPL.ArrayFunctionOperator
import qualified TinyAPL.Glyphs as G

io = Nilad (Just $ pure $ scalar $ Number 1) Nothing (G.quad : "io")

core = Quads
  ((\x -> (niladRepr x, x)) <$>
  [ io ])
  ((\x -> (dfnRepr x, x)) <$>
  [])
  ((\x -> (adverbRepr x, x)) <$>
  [])
  ((\x -> (conjRepr x, x)) <$>
  [])
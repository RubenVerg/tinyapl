module TinyAPL.CoreQuads where

import TinyAPL.ArrayFunctionOperator
import qualified TinyAPL.Glyphs as G
import TinyAPL.Interpreter

import Data.Complex
import Control.Monad.State

io = Nilad (Just $ pure $ scalar $ Number 1) Nothing (G.quad : "io")
ct = Nilad (Just $ pure $ scalar $ Number $ comparisonTolerance :+ 0) Nothing (G.quad : "ct")
u = Nilad (Just $ pure $ vector $ Character <$> ['A'..'Z']) Nothing (G.quad : "u")
l = Nilad (Just $ pure $ vector $ Character <$> ['a'..'z']) Nothing (G.quad : "l")
d = Nilad (Just $ pure $ vector $ Character <$> ['0'..'9']) Nothing (G.quad : "d")

exists = DefinedFunction (Just $ \x -> do
  let var = show x
  scope <- get
  case scopeLookup var scope of
    Just _ -> return $ scalar $ Number 1
    Nothing -> return $ scalar $ Number 0
  ) Nothing (G.quad : "Exists")
repr = DefinedFunction (Just $ \x -> return $ vector $ Character <$> arrayRepr x) Nothing (G.quad : "Repr")

core = Quads
  ((\x -> (niladRepr x, x)) <$>
  [ io, ct, u, l, d ])
  ((\x -> (dfnRepr x, x)) <$>
  [ exists, repr ])
  ((\x -> (adverbRepr x, x)) <$>
  [])
  ((\x -> (conjRepr x, x)) <$>
  [])
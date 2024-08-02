module TinyAPL.CoreQuads where

import TinyAPL.ArrayFunctionOperator
import qualified TinyAPL.Glyphs as G
import TinyAPL.Interpreter
import TinyAPL.Random

import Data.Complex
import Control.Monad.State
import Control.Monad.Error.Class
import TinyAPL.Error (Error(DomainError))
import Data.Time.Clock.POSIX

io = Nilad (Just $ pure $ scalar $ Number 1) Nothing (G.quad : "io")
ct = Nilad (Just $ pure $ scalar $ Number $ comparisonTolerance :+ 0) Nothing (G.quad : "ct")
u = Nilad (Just $ pure $ vector $ Character <$> ['A'..'Z']) Nothing (G.quad : "u")
l = Nilad (Just $ pure $ vector $ Character <$> ['a'..'z']) Nothing (G.quad : "l")
d = Nilad (Just $ pure $ vector $ Character <$> ['0'..'9']) Nothing (G.quad : "d")
seed = Nilad Nothing (Just $ \x -> do
  let e = DomainError "Seed must be a scalar integer"
  s <- liftEither (asScalar e x) >>= liftEither . asNumber e >>= liftEither . asInt e
  setSeed s) (G.quad : "seed")
ts = Nilad (Just $ do
  scalar . Number . realToFrac <$> liftIO getPOSIXTime
  ) Nothing (G.quad : "ts")

exists = Function (Just $ \x -> do
  let var = show x
  scope <- gets contextScope
  case scopeLookup var scope of
    Just _ -> return $ scalar $ Number 1
    Nothing -> return $ scalar $ Number 0
  ) Nothing (G.quad : "Exists")
repr = Function (Just $ \x -> return $ vector $ Character <$> arrayRepr x) Nothing (G.quad : "Repr")

core = Quads
  ((\x -> (niladRepr x, x)) <$>
  [ io, ct, u, l, d, seed, ts ])
  ((\x -> (functionRepr x, x)) <$>
  [ exists, repr ])
  ((\x -> (adverbRepr x, x)) <$>
  [])
  ((\x -> (conjRepr x, x)) <$>
  [])

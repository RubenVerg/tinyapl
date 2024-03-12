{-# LANGUAGE NegativeLiterals #-}

module TinyAPL.PrimitivesSpec where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import qualified TinyAPL.Primitives as P

import Test.Hspec
import Data.Complex

spec :: Spec
spec = do
  let scope = Scope [("l", vector $ Character <$> ['a'..'z'])] [] [] [] Nothing

  let m :: Function -> Array -> IO (Result Array)
      m fn y = runResult $ fst <$> runSt (callMonad fn y) scope

      d :: Function -> Array -> Array -> IO (Result Array)
      d fn x y = runResult $ fst <$> runSt (callDyad fn x y) scope

  describe "arrays" $ do
    describe [G.zilde] $ do
      it "is an empty vector" $ do
        P.zilde `shouldBe` vector []
  
  describe "functions" $ do
    describe [G.plus] $ do
      describe "conjugate" $ do
        it "doesn't change real numbers" $ do
          m P.plus (vector [Number 3, Number -2]) `shouldReturn` pure (vector [Number 3, Number -2])
        it "conjugates complex numbers" $ do
          m P.plus (vector [Number (3 :+ 2), Number (1 :+ -1)]) `shouldReturn` pure (vector [Number (3 :+ -2), Number (1 :+ 1)])
      describe "add" $ do
        it "adds complex numbers" $ do
          d P.plus (scalar $ Number (2 :+ 1)) (vector [Number 1, Number -3, Number (0 :+ 1)]) `shouldReturn` pure (vector [Number (3 :+ 1), Number (-1 :+ 1), Number (2 :+ 2)])
    
    describe [G.minus] $ do
      describe "negate" $ do
        it "negates complex numbers" $ do
          m P.minus (vector [Number 3, Number -1, Number (2 :+ 3)]) `shouldReturn` pure (vector [Number -3, Number 1, Number (-2 :+ -3)])
      describe "subtract" $ do
        it "subtracts complex numbers" $ do
          d P.minus (scalar $ Number 3) (vector [Number 1, Number -3, Number (2 :+ 1)]) `shouldReturn` pure (vector [Number 2, Number 6, Number (1 :+ -1)])

    describe [G.times] $ do
      describe "signum" $ do
        it "returns the sign of real numbers" $ do
          m P.times (vector [Number 3, Number 0, Number -5]) `shouldReturn` pure (vector [Number 1, Number 0, Number -1])
        it "returns the direction of complex numbers" $ do
          m P.times (vector [Number (0 :+ 2), Number (1 :+ 1)]) `shouldReturn` pure (vector [Number (0 :+ 1), Number (sqrt 0.5 :+ sqrt 0.5)])
      describe "multiply" $ do
        it "multiplies complex numbers" $ do
          d P.times (scalar $ Number (2 :+ 1)) (vector [Number 2, Number -1, Number (3 :+ -1)]) `shouldReturn` pure (vector [Number (4 :+ 2), Number (-2 :+ -1), Number (7 :+ 1)])
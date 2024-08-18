{-# LANGUAGE NegativeLiterals #-}

module TinyAPL.ParserSpec where

import qualified TinyAPL.Glyphs as G
import TinyAPL.Parser

import Test.Hspec

import TinyAPL.Complex

spec :: Spec
spec = do
  describe "tokenize" $ do
    let tok = tokenize "<test>"

    it "ignores comments" $ do
      tok "⟃abc⟄ 1" `shouldBe` pure [[TokenNumber [1] emptyPos]]
      tok "2 ⍝ abc" `shouldBe` pure [[TokenNumber [2] emptyPos]]
      tok "1 ⍝ abc\n10" `shouldBe` pure [[TokenNumber [1] emptyPos, TokenNumber [10] emptyPos]]

    it "parses numbers" $ do
      tok "1" `shouldBe` pure [[TokenNumber [1] emptyPos]]
      tok "¯2" `shouldBe` pure [[TokenNumber [-2] emptyPos]]
      tok "1.5" `shouldBe` pure [[TokenNumber [1.5] emptyPos]]
      tok "¯3.25" `shouldBe` pure [[TokenNumber [-3.25] emptyPos]]
      tok "3⏨2" `shouldBe` pure [[TokenNumber [300] emptyPos]]
      tok "2.4⏨¯3" `shouldBe` pure [[TokenNumber [0.0024] emptyPos]]
      tok "3ᴊ2" `shouldBe` pure [[TokenNumber [3 :+ 2] emptyPos]]
      tok "¯2ᴊ1.5⏨2" `shouldBe` pure [[TokenNumber [-2 :+ 150] emptyPos]]

    it "parses number ties" $ do
      tok "1‿2" `shouldBe` pure [[TokenNumber [1, 2] emptyPos]]
      tok "¯1‿2ᴊ3‿0" `shouldBe` pure [[TokenNumber [-1, 2 :+ 3, 0] emptyPos]]
    
    it "parses character vectors" $ do
      tok "'abc'" `shouldBe` pure [[TokenChar "abc" emptyPos]]
      tok "''" `shouldBe` pure [[TokenChar "" emptyPos]]
    
    it "parses strings" $ do
      tok "\"abc\"" `shouldBe` pure [[TokenString "abc" emptyPos]]
      tok "\"\"" `shouldBe` pure [[TokenString "" emptyPos]]
      tok "\"a⍘nb⍘\"c⍘⍘d⍘re⍘tf\"" `shouldBe` pure [[TokenString "a\nb\"c⍘d\re\tf" emptyPos]]

    it "parses vector notation" $ do
      tok "⟨1⋄2⟩" `shouldBe` pure [[TokenVector [[TokenNumber [1] emptyPos], [TokenNumber [2] emptyPos]] emptyPos]]
      tok "⟨⟩" `shouldBe` pure [[TokenVector [] emptyPos]]

    it "parses high rank notation" $ do
      tok "[1⋄2]" `shouldBe` pure [[TokenHighRank [[TokenNumber [1] emptyPos], [TokenNumber [2] emptyPos]] emptyPos]]
      tok "[]" `shouldBe` pure [[TokenHighRank [] emptyPos]]

    it "parses primitives" $ do
      mapM_ (\n -> tok [n] `shouldBe` pure [[TokenPrimArray n emptyPos]]) G.arrays
      mapM_ (\n -> tok [n] `shouldBe` pure [[TokenPrimFunction n emptyPos]]) G.functions
      mapM_ (\n -> tok [n] `shouldBe` pure [[TokenPrimAdverb n emptyPos]]) G.adverbs
      mapM_ (\n -> tok [n] `shouldBe` pure [[TokenPrimConjunction n emptyPos]]) G.conjunctions

    it "parses array names" $ do
      tok "abc ∆x" `shouldBe` pure [[TokenArrayName "abc" emptyPos, TokenArrayName "∆x" emptyPos]]
      tok "⍺ ⍺⍺ ⍵ ⍵⍵ ⎕ ⍞" `shouldBe` pure [[TokenArrayName "⍺" emptyPos, TokenArrayName "⍺⍺" emptyPos, TokenArrayName "⍵" emptyPos, TokenArrayName "⍵⍵" emptyPos, TokenArrayName "⎕" emptyPos, TokenArrayName "⍞" emptyPos]]
      tok "⎕io" `shouldBe` pure [[TokenArrayName "⎕io" emptyPos]]

    it "parses function names" $ do
      tok "Abc ⍙y" `shouldBe` pure [[TokenFunctionName "Abc" emptyPos, TokenFunctionName "⍙y" emptyPos]]
      tok "⍶⍶ ⍹⍹ ∇" `shouldBe` pure [[TokenFunctionName "⍶⍶" emptyPos, TokenFunctionName "⍹⍹" emptyPos, TokenFunctionName "∇" emptyPos]]
      tok "⎕C" `shouldBe` pure [[TokenFunctionName "⎕C" emptyPos]]
  
    it "parses adverb names" $ do
      tok "_Abc _abc" `shouldBe` pure [[TokenAdverbName "_Abc" emptyPos, TokenAdverbName "_abc" emptyPos]]
      tok "_∇" `shouldBe` pure [[TokenAdverbName "_∇" emptyPos]]
      tok "⎕_BinFile" `shouldBe` pure [[TokenAdverbName "⎕_BinFile" emptyPos]]

    it "parses conjunction names" $ do
      tok "_Abc_ _abc_" `shouldBe` pure [[TokenConjunctionName "_Abc_" emptyPos, TokenConjunctionName "_abc_" emptyPos]]
      tok "_∇_" `shouldBe` pure [[TokenConjunctionName "_∇_" emptyPos]]
      tok "⎕_Whatever_" `shouldBe` pure [[TokenConjunctionName "⎕_Whatever_" emptyPos]]

    it "parses qualified names" $ do
      tok "a→b→c" `shouldBe` pure [[TokenQualifiedArrayName (TokenArrayName "a" emptyPos) ["b", "c"] emptyPos]]
      tok "a→b→C" `shouldBe` pure [[TokenQualifiedFunctionName (TokenArrayName "a" emptyPos) ["b", "C"] emptyPos]]
      tok "a→b→_C" `shouldBe` pure [[TokenQualifiedAdverbName (TokenArrayName "a" emptyPos) ["b", "_C"] emptyPos]]
      tok "a→b→_C_" `shouldBe` pure [[TokenQualifiedConjunctionName (TokenArrayName "a" emptyPos) ["b", "_C_"] emptyPos]]

    it "parses assignment" $ do
      tok "abc←3" `shouldBe` pure [[TokenArrayAssign "abc" [TokenNumber [3] emptyPos] emptyPos]]
      tok "Abc←3" `shouldBe` pure [[TokenFunctionAssign "Abc" [TokenNumber [3] emptyPos] emptyPos]]
      tok "_Abc←3" `shouldBe` pure [[TokenAdverbAssign "_Abc" [TokenNumber [3] emptyPos] emptyPos]]
      tok "_Abc_←3" `shouldBe` pure [[TokenConjunctionAssign "_Abc_" [TokenNumber [3] emptyPos] emptyPos]]
      tok "⎕seed←3" `shouldBe` pure [[TokenArrayAssign "⎕seed" [TokenNumber [3] emptyPos] emptyPos]]

    it "parses qualified assignment" $ do
      tok "a→b→c←3" `shouldBe` pure [[TokenQualifiedArrayAssign (TokenArrayName "a" emptyPos) ["b", "c"] [TokenNumber [3] emptyPos] emptyPos]]
      tok "a→b→C←3" `shouldBe` pure [[TokenQualifiedFunctionAssign (TokenArrayName "a" emptyPos) ["b", "C"] [TokenNumber [3] emptyPos] emptyPos]]
      tok "a→b→_C←3" `shouldBe` pure [[TokenQualifiedAdverbAssign (TokenArrayName "a" emptyPos) ["b", "_C"] [TokenNumber [3] emptyPos] emptyPos]]
      tok "a→b→_C_←3" `shouldBe` pure [[TokenQualifiedConjunctionAssign (TokenArrayName "a" emptyPos) ["b", "_C_"] [TokenNumber [3] emptyPos] emptyPos]]

    it "parses dfns and dops" $ do
      tok "{3⋄1}" `shouldBe` pure [[TokenDfn [[TokenNumber [3] emptyPos], [TokenNumber [1] emptyPos]] emptyPos]]
      tok "_{3⋄1}" `shouldBe` pure [[TokenDadv [[TokenNumber [3] emptyPos], [TokenNumber [1] emptyPos]] emptyPos]]
      tok "_{3⋄1}_" `shouldBe` pure [[TokenDconj [[TokenNumber [3] emptyPos], [TokenNumber [1] emptyPos]] emptyPos]]

    it "parses wraps" $ do
      tok "(□+)" `shouldBe` pure [[TokenWrap [TokenPrimFunction '+' emptyPos] emptyPos]]

    it "parses unwraps" $ do
      tok "(⊏3)" `shouldBe` pure [[TokenUnwrap [TokenNumber [3] emptyPos] emptyPos]]
    
    it "parses guards" $ do
      tok "{1:2}" `shouldBe` pure [[TokenDfn [[TokenGuard [TokenNumber [1] emptyPos] [TokenNumber [2] emptyPos] emptyPos]] emptyPos]]

    it "parses exit statements" $ do
      tok "{■5}" `shouldBe` pure [[TokenDfn [[TokenExit [TokenNumber [5] emptyPos] emptyPos]] emptyPos]]

    it "parses separator-separated statements" $ do
      tok "1⋄2" `shouldBe` pure [[TokenNumber [1] emptyPos], [TokenNumber [2] emptyPos]]

    it "ignores spaces and newlines" $ do
      tok "1\n2" `shouldBe` pure [[TokenNumber [1] emptyPos, TokenNumber [2] emptyPos]]
      tok "1     2" `shouldBe` pure [[TokenNumber [1] emptyPos, TokenNumber [2] emptyPos]]
    
    it "parses parens" $ do
      tok "(1 2)" `shouldBe` pure [[TokenParens [TokenNumber [1] emptyPos, TokenNumber [2] emptyPos] emptyPos]]

    it "parses trains and modifier trains" $ do
      tok "⦅1⋄2⦆" `shouldBe` pure [[TokenTrain [Just $ [TokenNumber [1] emptyPos], Just $ [TokenNumber [2] emptyPos]] emptyPos]]
      tok "⦅1⋄2⋄3⦆" `shouldBe` pure [[TokenTrain [Just $ [TokenNumber [1] emptyPos], Just $ [TokenNumber [2] emptyPos], Just $ [TokenNumber [3] emptyPos]] emptyPos]]
      tok "⦅1⋄⋄2⋄3⦆" `shouldBe` pure [[TokenTrain [Just $ [TokenNumber [1] emptyPos], Nothing, Just $ [TokenNumber [2] emptyPos], Just $ [TokenNumber [3] emptyPos]] emptyPos]]
      tok "_⦅1⋄2⋄3⦆" `shouldBe` pure [[TokenAdverbTrain [Just $ [TokenNumber [1] emptyPos], Just $ [TokenNumber [2] emptyPos], Just $ [TokenNumber [3] emptyPos]] emptyPos]]
      tok "_⦅1⋄2⋄3⦆_" `shouldBe` pure [[TokenConjunctionTrain [Just $ [TokenNumber [1] emptyPos], Just $ [TokenNumber [2] emptyPos], Just $ [TokenNumber [3] emptyPos]] emptyPos]]

    it "parses destructuring assignment" $ do
      tok "⟨a⋄b⟩←9" `shouldBe` pure [[TokenVectorAssign ["a", "b"] [TokenNumber [9] emptyPos] emptyPos]]
      tok "[a⋄b]←7" `shouldBe` pure [[TokenHighRankAssign ["a", "b"] [TokenNumber [7] emptyPos] emptyPos]]

    it "parses structs" $ do
      tok "⦃1⋄2⋄3⦄" `shouldBe` pure [[TokenStruct [[TokenNumber [1] emptyPos], [TokenNumber [2] emptyPos], [TokenNumber [3] emptyPos]] emptyPos]]

  describe "binder" $ do
    let e2m (Right x) = Just x
        e2m (Left _)  = Nothing
    let par = e2m . parse "<test>"
    
    it "parses leaves" $ do
      par "1" `shouldBe` pure [Leaf CatArray (TokenNumber [1] emptyPos)]
      par "'abc'" `shouldBe` pure [Leaf CatArray (TokenChar "abc" emptyPos)]
      par "\"abc\"" `shouldBe` pure [Leaf CatArray (TokenString "abc" emptyPos)]
      par "⍬" `shouldBe` pure [Leaf CatArray (TokenPrimArray '⍬' emptyPos)]
      par "+" `shouldBe` pure [Leaf CatFunction (TokenPrimFunction '+' emptyPos)]
      par "⍨" `shouldBe` pure [Leaf CatAdverb (TokenPrimAdverb '⍨' emptyPos)]
      par "∘" `shouldBe` pure [Leaf CatConjunction (TokenPrimConjunction '∘' emptyPos)]
      par "abc" `shouldBe` pure [Leaf CatArray (TokenArrayName "abc" emptyPos)]
      par "Abc" `shouldBe` pure [Leaf CatFunction (TokenFunctionName "Abc" emptyPos)]
      par "_Abc" `shouldBe` pure [Leaf CatAdverb (TokenAdverbName "_Abc" emptyPos)]
      par "_Abc_" `shouldBe` pure [Leaf CatConjunction (TokenConjunctionName "_Abc_" emptyPos)]
    
    it "parses parens" $ do
      par "(1)" `shouldBe` pure [Leaf CatArray (TokenNumber [1] emptyPos)]

    describe "application" $ do
      it "parses monad application" $ do
        par "+1" `shouldBe` pure [MonadCallBranch (Leaf CatFunction (TokenPrimFunction '+' emptyPos)) (Leaf CatArray (TokenNumber [1] emptyPos))]
      
      it "parses dyad application" $ do
        par "1+" `shouldBe` pure [DyadCallBranch (Leaf CatArray (TokenNumber [1] emptyPos)) (Leaf CatFunction (TokenPrimFunction '+' emptyPos))]
        par "1+2" `shouldBe` pure [MonadCallBranch (DyadCallBranch (Leaf CatArray (TokenNumber [1] emptyPos)) (Leaf CatFunction (TokenPrimFunction '+' emptyPos))) (Leaf CatArray (TokenNumber [2] emptyPos))]

      it "parses adverb application" $ do
        par "1⍨" `shouldBe` pure [AdverbCallBranch (Leaf CatArray (TokenNumber [1] emptyPos)) (Leaf CatAdverb (TokenPrimAdverb '⍨' emptyPos))]
        par "+⍨" `shouldBe` pure [AdverbCallBranch (Leaf CatFunction (TokenPrimFunction '+' emptyPos)) (Leaf CatAdverb (TokenPrimAdverb '⍨' emptyPos))]

      it "parses conjunction application" $ do
        par "∘1" `shouldBe` pure [ConjunctionCallBranch (Leaf CatConjunction (TokenPrimConjunction '∘' emptyPos)) (Leaf CatArray (TokenNumber [1] emptyPos))]
        par "∘+" `shouldBe` pure [ConjunctionCallBranch (Leaf CatConjunction (TokenPrimConjunction '∘' emptyPos)) (Leaf CatFunction (TokenPrimFunction '+' emptyPos))]

    describe "dfns" $ do
      it "parses dfns and dops" $ do
        par "{1}" `shouldBe` pure [DefinedBranch CatFunction [Leaf CatArray (TokenNumber [1] emptyPos)]]
        par "_{1}" `shouldBe` pure [DefinedBranch CatAdverb [Leaf CatArray (TokenNumber [1] emptyPos)]]
        par "_{1}_" `shouldBe` pure [DefinedBranch CatConjunction [Leaf CatArray (TokenNumber [1] emptyPos)]]
      
      it "requires at least one statement" $ do
        par "{}" `shouldBe` Nothing

      it "requires the last statement to be an array" $ do
        par "{+}" `shouldBe` Nothing
    
    describe "assignment" $ do
      it "parses assignment to variables of the correct type" $ do
        par "a←b" `shouldBe` pure [AssignBranch CatArray "a" (Leaf CatArray (TokenArrayName "b" emptyPos))]
        par "A←B" `shouldBe` pure [AssignBranch CatFunction "A" (Leaf CatFunction (TokenFunctionName "B" emptyPos))]
        par "_A←_B" `shouldBe` pure [AssignBranch CatAdverb "_A" (Leaf CatAdverb (TokenAdverbName "_B" emptyPos))]
        par "_A_←_B_" `shouldBe` pure [AssignBranch CatConjunction "_A_" (Leaf CatConjunction (TokenConjunctionName "_B_" emptyPos))]

      it "fails on assignment to variables of the wrong type" $ do
        par "a←B" `shouldBe` Nothing
        par "A←_B_" `shouldBe` Nothing
        par "_A←b" `shouldBe` Nothing
        par "_A_←_B" `shouldBe` Nothing
    
    describe "guards" $ do
      it "parses guards with array conditions" $ do
        par "{a:b}" `shouldBe` pure [DefinedBranch CatFunction [GuardBranch (Leaf CatArray (TokenArrayName "a" emptyPos)) (Leaf CatArray (TokenArrayName "b" emptyPos))]]
      
      it "fails on guards with non-array conditions" $ do
        par "{A:b}" `shouldBe` Nothing

    describe "exit statements" $ do
      it "parses exit statements with array results" $ do
        par "{■3}" `shouldBe` pure [DefinedBranch CatFunction [ExitBranch (Leaf CatArray (TokenNumber [3] emptyPos))]]

      it "fails on exit statements with non-array results" $ do
        par "{■+}" `shouldBe` Nothing

    describe "vector notation" $ do
      it "parses arrays with array or function contents" $ do
        par "⟨1⋄2⟩" `shouldBe` pure [VectorBranch [Leaf CatArray (TokenNumber [1] emptyPos), Leaf CatArray (TokenNumber [2] emptyPos)]]
        par "⟨+⟩" `shouldBe` pure [VectorBranch [Leaf CatFunction (TokenPrimFunction '+' emptyPos)]]
        par "⟨⟩" `shouldBe` pure [VectorBranch []]
      
      it "fails on vector notation with non-array nor function contents" $ do
        par "⟨¨⟩" `shouldBe` Nothing

    describe "high rank notation" $ do
      it "parses arrays with array contents" $ do
        par "[1⋄2]" `shouldBe` pure [HighRankBranch [Leaf CatArray (TokenNumber [1] emptyPos), Leaf CatArray (TokenNumber [2] emptyPos)]]
        par "[]" `shouldBe` pure [HighRankBranch []]
      
      it "fails on arrays with non-array contents" $ do
        par "[+]" `shouldBe` Nothing

    describe "wraps" $ do
      it "parses wraps of functions" $ do
        par "(□+)" `shouldBe` pure [WrapBranch (Leaf CatFunction (TokenPrimFunction '+' emptyPos))]

      it "fails on wraps of non-functions" $ do
        par "(□3)" `shouldBe` Nothing

    describe "unwraps" $ do
      it "parses unwraps of arrays" $ do
        par "(⊏3)" `shouldBe` pure [UnwrapBranch (Leaf CatArray (TokenNumber [3] emptyPos))]

      it "fails on unwraps of non-arrays" $ do
        par "(⊏+)" `shouldBe` Nothing

    describe "structs" $ do
      it "parses structs" $ do
        par "⦃1⋄2⋄3⦄" `shouldBe` pure [StructBranch [Leaf CatArray (TokenNumber [1] emptyPos), Leaf CatArray (TokenNumber [2] emptyPos), Leaf CatArray (TokenNumber [3] emptyPos)]]

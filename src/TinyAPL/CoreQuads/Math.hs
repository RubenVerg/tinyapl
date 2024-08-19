{-# LANGUAGE FlexibleContexts #-}

module TinyAPL.CoreQuads.Math (math) where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State (MonadState(get))

piA :: Array
piA = scalar $ Number $ pi

complementaryS :: MonadError Error m => ScalarValue -> m ScalarValue
complementaryS (Number y) = pure $ Number $ sqrt $ 1 - y * y
complementaryS _ = throwError $ DomainError "Complementary argument must be a number"

complementaryF :: Function
complementaryF = Function (Just $ scalarMonad complementaryS) Nothing "Complementary" Nothing

sinS :: MonadError Error m => ScalarValue -> m ScalarValue
sinS (Number y) = pure $ Number $ sin y
sinS _ = throwError $ DomainError "Sine argument must be a number"

sinF :: Function
sinF = Function (Just $ scalarMonad sinS) Nothing "Sin" Nothing

arcsinS :: MonadError Error m => ScalarValue -> m ScalarValue
arcsinS (Number y) = pure $ Number $ asin y
arcsinS _ = throwError $ DomainError "Arcsine argument must be a number"

arcsinF :: Function
arcsinF = Function (Just $ scalarMonad arcsinS) Nothing "Arcsin" Nothing

cosS :: MonadError Error m => ScalarValue -> m ScalarValue
cosS (Number y) = pure $ Number $ cos y
cosS _ = throwError $ DomainError "Cosine argument must be a number"

cosF :: Function
cosF = Function (Just $ scalarMonad cosS) Nothing "Cos" Nothing

arccosS :: MonadError Error m => ScalarValue -> m ScalarValue 
arccosS (Number y) = pure $ Number $ acos y
arccosS _ = throwError $ DomainError "Arccosine argument must be a number"

arccosF :: Function
arccosF = Function (Just $ scalarMonad arccosS) Nothing "Arccos" Nothing

tanS :: MonadError Error m => ScalarValue -> m ScalarValue  
tanS (Number y) = pure $ Number $ tan y
tanS _ = throwError $ DomainError "Tangent argument must be a number"

tanF :: Function
tanF = Function (Just $ scalarMonad tanS) Nothing "Tan" Nothing

arctanS :: MonadError Error m => ScalarValue -> m ScalarValue  
arctanS (Number y) = pure $ Number $ atan y
arctanS _ = throwError $ DomainError "Arctangent argument must be a number"

arctanF :: Function
arctanF = Function (Just $ scalarMonad arctanS) Nothing "Arctan" Nothing

sinhS :: MonadError Error m => ScalarValue -> m ScalarValue  
sinhS (Number y) = pure $ Number $ sinh y
sinhS _ = throwError $ DomainError "Hyperbolic sine argument must be a number"

sinhF :: Function
sinhF = Function (Just $ scalarMonad sinhS) Nothing "Sinh" Nothing

arsinhS :: MonadError Error m => ScalarValue -> m ScalarValue  
arsinhS (Number y) = pure $ Number $ asinh y
arsinhS _ = throwError $ DomainError "Hyperbolic arsine argument must be a number"

arsinhF :: Function
arsinhF = Function (Just $ scalarMonad arsinhS) Nothing "Arsinh" Nothing

coshS :: MonadError Error m => ScalarValue -> m ScalarValue  
coshS (Number y) = pure $ Number $ cosh y
coshS _ = throwError $ DomainError "Hyperbolic cosine argument must be a number"

coshF :: Function
coshF = Function (Just $ scalarMonad coshS) Nothing "Cosh" Nothing

arcoshS :: MonadError Error m => ScalarValue -> m ScalarValue  
arcoshS (Number y) = pure $ Number $ acosh y
arcoshS _ = throwError $ DomainError "Hyperbolic arcosine argument must be a number"

arcoshF :: Function
arcoshF = Function (Just $ scalarMonad arcoshS) Nothing "Arcosh" Nothing

tanhS :: MonadError Error m => ScalarValue -> m ScalarValue  
tanhS (Number y) = pure $ Number $ tanh y
tanhS _ = throwError $ DomainError "Hyperbolic tangent argument must be a number"

tanhF :: Function
tanhF = Function (Just $ scalarMonad tanhS) Nothing "Tanh" Nothing

artanhS :: MonadError Error m => ScalarValue -> m ScalarValue  
artanhS (Number y) = pure $ Number $ atanh y
artanhS _ = throwError $ DomainError "Hyperbolic artangent argument must be a number"

artanhF :: Function
artanhF = Function (Just $ scalarMonad artanhS) Nothing "Artanh" Nothing

math = Nilad (Just $ do
  scope <- createRef (Scope [("pi", piA)] ((\n -> (functionRepr n, n)) <$>
    [ complementaryF
    , sinF
    , arcsinF
    , cosF
    , arccosF
    , tanF
    , arctanF
    , sinhF
    , arsinhF
    , coshF
    , arcoshF
    , tanhF
    , artanhF
    ]) [] [] Nothing)
  ctx <- get
  pure $ scalar $ Struct $ ctx{ contextScope = scope } ) Nothing (G.quad : "math") Nothing
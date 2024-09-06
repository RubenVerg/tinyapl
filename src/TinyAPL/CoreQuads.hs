{-# LANGUAGE LambdaCase #-}
module TinyAPL.CoreQuads where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.CoreQuads.Math
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import TinyAPL.Interpreter
import TinyAPL.Random
import TinyAPL.StandardLibrary

import TinyAPL.Complex
import Control.Monad.State
import Data.Time.Clock.POSIX
import Control.Concurrent
import Data.List
import Data.List.Split (splitOn)

io = Nilad (Just $ pure $ scalar $ Number 1) Nothing (G.quad : "io") Nothing
ct = Nilad (Just $ pure $ scalar $ Number $ comparisonTolerance :+ 0) Nothing (G.quad : "ct") Nothing
u = Nilad (Just $ pure $ vector $ Character <$> ['A'..'Z']) Nothing (G.quad : "u") Nothing
l = Nilad (Just $ pure $ vector $ Character <$> ['a'..'z']) Nothing (G.quad : "l") Nothing
d = Nilad (Just $ pure $ vector $ Character <$> ['0'..'9']) Nothing (G.quad : "d") Nothing
seed = Nilad Nothing (Just $ \x -> do
  let e = DomainError "Seed must be a scalar integer"
  s <- asScalar e x >>= asNumber e >>= asInt e
  setSeed s) (G.quad : "seed") Nothing
unix = Nilad (Just $ scalar . Number . realToFrac <$> liftToSt getPOSIXTime) Nothing (G.quad : "unix") Nothing
ts = Nilad (Just $ do
  err <- gets contextErr
  err "Deprecation warning: ⎕ts has been replaced by ⎕unix and will be used for something else in a future version\n"
  scalar . Number . realToFrac <$> liftToSt getPOSIXTime) Nothing (G.quad : "ts") Nothing

exists = Function (Just $ \y -> do
  var <- asString (DomainError "Exists argument must be a string") y
  v <- gets contextScope >>= readRef >>= scopeLookup True var
  case v of
    Just _ -> return $ scalar $ Number 1
    Nothing -> return $ scalar $ Number 0
  ) (Just $ \x y -> do
  var <- asString (DomainError "Exists right argument must be a string") y
  let err = DomainError "Exists left argument must be a scalar struct"
  ns <- asScalar err x >>= asStruct err >>= readRef . contextScope
  case scopeShallowLookup False var ns of
    Just _ -> pure $ scalar $ Number 1
    Nothing -> pure $ scalar $ Number 0) (G.quad : "Exists") Nothing
repr = Function (Just $ \y -> return $ vector $ Character <$> arrayRepr y) Nothing (G.quad : "Repr") Nothing
delay = Function (Just $ \y -> do
  let err = DomainError "Delay argument must be a nonnegative scalar number"
  n <- asScalar err y >>= asNumber err >>= asReal err
  if n < 0 then throwError err else do
    start <- realToFrac <$> liftToSt getPOSIXTime
    liftToSt $ threadDelay $ floor $ n * 1000 * 1000
    end <- realToFrac <$> liftToSt getPOSIXTime
    pure $ scalar $ Number $ (end - start) :+ 0
  ) Nothing (G.quad : "Delay") Nothing
type_ = Function (Just $ \(Array sh cs) -> return $ Array sh $ (\case
  Number _ -> Number 0
  Character _ -> Number 1
  Box _ -> Number 2
  Wrap _ -> Number 3
  AdverbWrap _ -> Number 4
  ConjunctionWrap _ -> Number 5
  Struct _ -> Number 6) <$> cs) Nothing (G.quad : "Type") Nothing

core = quadsFromReprs [ io, ct, u, l, d, seed, unix, ts, math ] [ exists, repr, delay, type_ ] [] []

makeImport :: (FilePath -> St String) -> Maybe ([String] -> St String) -> Function
makeImport read readStd = Function (Just $ \x -> do
  let err = DomainError "Import argument must be a character vector"
  path <- asVector err x >>= mapM (asCharacter err)
  ctx <- getContext
  scope <- createRef $ Scope [] [] [] [] Nothing -- The scope has intentionally no parent; imports run in an isolated context
  let ctx' = ctx{ contextScope = scope }
  source <-
    if isPrefixOf "std:" path
    then case readStd of
      Just fn -> fn <$> splitOn "/" $ drop (length "std:") path
      Nothing -> case lookup (splitOn "/" $ drop (length "std:") path) standardLibrary of
        Just source -> pure source
        Nothing -> throwError $ DomainError $ "Standard library module " ++ path ++ " not found"
    else read path
  runWithContext ctx' $ run' path source
  pure $ scalar $ Struct ctx') Nothing (G.quad : "Import") Nothing

makeSystemInfo :: String -> String -> Bool -> Nilad
makeSystemInfo os arch js = Nilad (Just $ do
  scope <- createRef $ Scope [ ("os", (VariableConstant, vector $ Character <$> os))
                             , ("arch", (VariableConstant, vector $ Character <$> arch))
                             , ("js", (VariableConstant, scalar $ boolToScalar js))] [] [] [] Nothing
  ctx <- get
  pure $ scalar $ Struct $ ctx{ contextScope = scope } ) Nothing (G.quad : "systemInfo") Nothing

module TinyAPL.Quads.File where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G

import Control.Monad.Catch
import Control.Exception (IOException)

read :: FilePath -> St String
read path = liftToSt (readFile path) `catch` (\e -> throwError (IOError $ show (e :: IOException)))

read' :: Noun -> St Noun
read' s = do
  let err = DomainError "read: argument must be a string path"
  path <- asString err s
  vector . fmap Character <$> TinyAPL.Quads.File.read path

write :: FilePath -> String -> St ()
write path d = liftToSt (writeFile path d) `catch` (\e -> throwError (IOError $ show (e :: IOException)))

write' :: Noun -> Noun -> St Noun
write' s d = do
  let err = DomainError "write: arguments must be a string path and string data"
  path <- asString err s
  da <- asString err d
  write path da
  pure $ vector []

file :: Nilad
file = Nilad (Just $ do
  scope <- createRef $ Scope [] ((\n -> (functionRepr n, (VariableConstant, n))) <$>
    [ PrimitiveFunction (Just read') Nothing "Read" Nothing
    , PrimitiveFunction Nothing (Just write') "Write" Nothing]) [] [] Nothing
  ctx <- getContext
  pure $ scalar $ Struct $ ctx{ contextScope = scope }) Nothing (G.quad : "file") Nothing
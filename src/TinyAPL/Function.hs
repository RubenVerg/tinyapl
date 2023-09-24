module TinyAPL.Function where
import TinyAPL.Array
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G

data Function
  = DefinedFunction
    { dfnMonad :: Maybe (Array -> Result Array)
    , dfnDyad  :: Maybe (Array -> Array -> Result Array)
    , dfnRepr  :: String }
  | Atop { atopLeft :: Function, atopRight :: Function }
  | Over { overLeft :: Function, overRight :: Function }

instance Show Function where
  show (DefinedFunction { dfnRepr = repr }) = "<fn " ++ repr ++ ">"
  show (l `Atop` r) = "(" ++ show l ++ [G.atop] ++ show r ++ ")"
  show (l `Over` r) = "(" ++ show l ++ [G.over] ++ show r ++ ")"

callMonad :: Function -> Array -> Result Array
callMonad (DefinedFunction (Just f) _ _) x = f x
callMonad f@(DefinedFunction Nothing _ _) _ = err $ DomainError $ "Function " ++ show f ++ " cannot be called monadically."
callMonad (f `Atop` g) x = callMonad g x >>= callMonad f
callMonad (f `Over` g) x = callMonad g x >>= callMonad f

callDyad :: Function -> Array -> Array -> Result Array
callDyad (DefinedFunction _ (Just g) _) a b = g a b
callDyad f@(DefinedFunction _ Nothing _) _ _ = err $ DomainError $ "Function " ++ show f ++ " cannot be called dyadically."
callDyad (f `Atop` g) a b = callDyad g a b >>= callMonad f
callDyad (f `Over` g) a b = do
  a' <- callMonad g a
  b' <- callMonad g b
  callDyad f a' b'
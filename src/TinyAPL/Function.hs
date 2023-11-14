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
  | After { afterLeft :: Function, afterRight :: Function }
  | Before { beforeLeft :: Function, beforeRight :: Function }
  | LeftHook { leftHookLeft :: Function, leftHookRight :: Function }
  | RightHook { rightHookLeft :: Function, rightHookRight :: Function }
  | Selfie { selfieFunction :: Function }
  | BindLeft { bindLeftArray :: Array, bindLeftFunction :: Function }
  | BindRight { bindRightFunction :: Function, bindRightArray :: Array }
  | DefaultBindLeft { defaultBindLeftArray :: Array, defaultBindLeftFunction :: Function }
  | DefaultBindRight { defaultBindRightFunction :: Function, defaultBindRightArray :: Array }
  | Constant { constantArray :: Array }

instance Show Function where
  show (DefinedFunction { dfnRepr = repr }) = "<fn " ++ repr ++ ">"
  show (l `Atop` r) = "(" ++ show l ++ [G.atop] ++ show r ++ ")"
  show (l `Over` r) = "(" ++ show l ++ [G.over] ++ show r ++ ")"
  show (l `After` r) = "(" ++ show l ++ [G.after] ++ show r ++ ")"
  show (l `Before` r) = "(" ++ show l ++ [G.before] ++ show r ++ ")"
  show (l `LeftHook` r) = "(" ++ show l ++ [G.leftHook] ++ show r ++ ")"
  show (l `RightHook` r) = "(" ++ show l ++ [G.rightHook] ++ show r ++ ")"
  show (Selfie f) = show f ++ [G.selfie]
  show (l `BindLeft` r) = "(" ++ show l ++ [G.after] ++ show r ++ ")"
  show (l `BindRight` r) = "(" ++ show l ++ [G.after] ++ show r ++ ")"
  show (l `DefaultBindLeft` r) = "(" ++ show l ++ [G.before] ++ show r ++ ")"
  show (l `DefaultBindRight` r) = "(" ++ show l ++ [G.before] ++ show r ++ ")"
  show (Constant x) = show x ++ [G.selfie]

callMonad :: Function -> Array -> Result Array
callMonad (DefinedFunction (Just f) _ _) x = f x
callMonad f@(DefinedFunction Nothing _ _) _ = err $ DomainError $ "Function " ++ show f ++ " cannot be called monadically."
callMonad (f `Atop` g) x = callMonad g x >>= callMonad f
callMonad (f `Over` g) x = callMonad g x >>= callMonad f
callMonad (f `After` g) x = callMonad g x >>= callMonad f
callMonad (f `Before` g) x = callMonad f x >>= callMonad g
callMonad (f `LeftHook` g) x = do
  x' <- callMonad f x
  callDyad g x' x
callMonad (f `RightHook` g) x = do
  x' <- callMonad g x
  callDyad f x x'
callMonad (Selfie f) x = callDyad f x x
callMonad (a `BindLeft` f) x = callDyad f a x
callMonad (f `BindRight` a) x = callDyad f x a
callMonad (a `DefaultBindLeft` f) x = callDyad f a x
callMonad (f `DefaultBindRight` a) x = callDyad f x a
callMonad (Constant x) _ = pure x

callDyad :: Function -> Array -> Array -> Result Array
callDyad (DefinedFunction _ (Just g) _) a b = g a b
callDyad f@(DefinedFunction _ Nothing _) _ _ = err $ DomainError $ "Function " ++ show f ++ " cannot be called dyadically."
callDyad (f `Atop` g) a b = callDyad g a b >>= callMonad f
callDyad (f `Over` g) a b = do
  a' <- callMonad g a
  b' <- callMonad g b
  callDyad f a' b'
callDyad (f `After` g) a b = do
  b' <- callMonad g b
  callDyad f a b'
callDyad (f `Before` g) a b = do
  a' <- callMonad f a
  callDyad g a' b
callDyad (f `LeftHook` g) a b = do
  a' <- callMonad f a
  callDyad g a' b
callDyad (f `RightHook` g) a b = do
  b' <- callMonad g b
  callDyad f a b'
callDyad (Selfie f) a b = callDyad f b a
callDyad (_ `BindLeft` _) _ _ = err $ DomainError "Bound function called dyadically"
callDyad (_ `BindRight` _) _ _ = err $ DomainError "Bound function called dyadically"
callDyad (_ `DefaultBindLeft` f) x y = callDyad f x y
callDyad (f `DefaultBindRight` _) x y = callDyad f x y
callDyad (Constant x) _ _ = pure x
module TinyAPL.Operator where
import TinyAPL.Error
import TinyAPL.Array
import TinyAPL.Function

data Adverb = Adverb
  { adverbOnArray    :: Maybe (Array    -> Result Function)
  , adverbOnFunction :: Maybe (Function -> Result Function)
  , adverbRepr       :: String }

instance Show Adverb where
  show (Adverb _ _ repr) = "<adverb " ++ repr ++ ">"

callOnArray :: Adverb -> Array -> Result Function
callOnArray (Adverb (Just op) _ _) x = op x
callOnArray adv _ = err $ DomainError $ "Operator " ++ show adv ++ " does not take array operands."

callOnFunction :: Adverb -> Function -> Result Function
callOnFunction (Adverb _ (Just op) _) x = op x
callOnFunction adv _ = err $ DomainError $ "Operator " ++ show adv ++ " does not take functions operands."

data Conjunction = Conjunction
  { conjOnArrayArray       :: Maybe (Array    -> Array    -> Result Function)
  , conjOnArrayFunction    :: Maybe (Array    -> Function -> Result Function)
  , conjOnFunctionArray    :: Maybe (Function -> Array    -> Result Function)
  , conjOnFunctionFunction :: Maybe (Function -> Function -> Result Function)
  , conjRepr               :: String }

instance Show Conjunction where
  show (Conjunction _ _ _ _ repr) = "<conj " ++ repr ++ ">"

callOnArrayAndArray :: Conjunction -> Array -> Array -> Result Function
callOnArrayAndArray (Conjunction (Just op) _ _ _ _) x y = op x y
callOnArrayAndArray conj _ _ = err $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to two arrays."

callOnArrayAndFunction :: Conjunction -> Array -> Function -> Result Function
callOnArrayAndFunction (Conjunction _ (Just op) _ _ _) x y = op x y
callOnArrayAndFunction conj _ _ = err $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to an array and a function."

callOnFunctionAndArray :: Conjunction -> Function -> Array -> Result Function
callOnFunctionAndArray (Conjunction _ _ (Just op) _ _) x y = op x y
callOnFunctionAndArray conj _ _ = err $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to a function and an array."

callOnFunctionAndFunction :: Conjunction -> Function -> Function -> Result Function
callOnFunctionAndFunction (Conjunction _ _ _ (Just op) _) x y = op x y
callOnFunctionAndFunction conj _ _ = err $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to two functions."

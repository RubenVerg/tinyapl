module TinyAPL.Error where
import GHC.Stack (HasCallStack)

data Error
  = DomainError String
  | LengthError String
  | RankError String
  | NYIError String
  deriving (Show)

type Result = Either Error

-- sadly we need this.
unerror :: HasCallStack => Result a -> a
unerror (Right x) = x
unerror (Left e) = error $ show e

err :: Error -> Result a
err = Left
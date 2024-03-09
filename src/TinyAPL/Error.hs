module TinyAPL.Error where
import GHC.Stack (HasCallStack)
import Control.Monad.Except

data Error
  = DomainError String
  | LengthError String
  | RankError String
  | NYIError String
  | SyntaxError String

instance Show Error where
  show err = let
    (errorName, errorMessage) = case err of
      (DomainError msg) -> ("Domain error", msg)
      (LengthError msg) -> ("Length error", msg)
      (RankError msg) -> ("Rank error", msg)
      (NYIError msg) -> ("Not yet implemented!", msg)
      (SyntaxError msg) -> ("Syntax error", msg)
    hasNewline = '\n' `elem` errorMessage
    in if hasNewline
      then errorName ++ '\n' : errorMessage
      else errorName ++ ": " ++ errorMessage

type Result = Either Error
type ResultIO = ExceptT Error IO

-- sadly we need this.
unerror :: HasCallStack => Result a -> a
unerror x = case x of
  Right x -> x
  Left  e -> error $ show e

err :: Error -> Result a
err = Left

except :: Monad m => Either e a -> ExceptT e m a
except m = ExceptT $ return m

runResult :: ResultIO a -> IO (Result a)
runResult = runExceptT
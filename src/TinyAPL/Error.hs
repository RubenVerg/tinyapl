{-# LANGUAGE FlexibleContexts #-}

module TinyAPL.Error where
import GHC.Stack (HasCallStack)
import Control.Monad.Except

data Error
  = UserError String
  | DomainError String
  | LengthError String
  | RankError String
  | NYIError String
  | SyntaxError String
  | AssertionError String
  deriving (Eq, Ord)

unreachable = AssertionError "Unreachable code reached!"

errorCode :: Error -> Int
errorCode (UserError _) = 1
errorCode (DomainError _) = 2
errorCode (LengthError _) = 3
errorCode (RankError _) = 4
errorCode (NYIError _) = 5
errorCode (SyntaxError _) = 6
errorCode (AssertionError _) = 7

errorMessage :: Error -> String
errorMessage (UserError e) = e
errorMessage (DomainError e) = e
errorMessage (LengthError e) = e
errorMessage (RankError e) = e
errorMessage (NYIError e) = e
errorMessage (SyntaxError e) = e
errorMessage (AssertionError e) = e

fromErrorCode :: Int -> String -> Error
fromErrorCode 1 = UserError
fromErrorCode 2 = DomainError
fromErrorCode 3 = LengthError
fromErrorCode 4 = RankError
fromErrorCode 5 = NYIError
fromErrorCode 6 = SyntaxError
fromErrorCode 7 = AssertionError
fromErrorCode _ = error "fromErrorCode: unknown error type"

instance Show Error where
  show err = let
    (errorName, errorMessage) = case err of
      UserError msg -> ("User error", msg)
      DomainError msg -> ("Domain error", msg)
      LengthError msg -> ("Length error", msg)
      RankError msg -> ("Rank error", msg)
      NYIError msg -> ("Not yet implemented!", msg)
      SyntaxError msg -> ("Syntax error", msg)
      AssertionError msg -> ("Assertion failed", msg)
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

except :: Monad m => Either e a -> ExceptT e m a
except m = ExceptT $ return m

liftEither :: MonadError e m => Either e a -> m a
liftEither = Control.Monad.Except.liftEither

throwError :: MonadError Error m => Error -> m a
throwError = Control.Monad.Except.throwError

runResult :: ResultIO a -> IO (Result a)
runResult = runExceptT
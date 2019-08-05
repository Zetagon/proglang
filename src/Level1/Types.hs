-- | Untyped Language

module Level1.Types where

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict

data EvalState = EvalState
                    { _evalSStack :: ![Expr]
                    , _evalSSEnv :: M.Map FNName [Expr] }
  deriving (Show)

type EvalStateM a = StateT EvalState IO a

data Expr = Word FNName
            | Quote [Expr]
            | NewStackQuote Int [Expr]
            | BuiltinWord (EvalStateM ())
            | Literal Value

instance Show Expr where
  show (Quote exprs) = "Quote: " ++ show exprs
  show (Word exprs) = "Word: " ++ show exprs
  show (Literal val) = show val
  show (BuiltinWord _) = "BuiltIn function"

instance Eq Expr where
  (Word expr) == (Word expr') = expr == expr'
  (Quote expr) == (Quote expr') = expr == expr'
  (Literal val) == (Literal val') = val == val'
  _ == _ = False

data Value = VInt !Int
           | VQuote [Expr]
           deriving (Show, Eq)


newtype FNName = FNName String
  deriving (Eq, Show, Ord)

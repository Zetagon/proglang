-- | Untyped Language

module Level1.Types where

data Expr = Word [Expr]
          | Quote [Expr]
          | BuiltinWord (Stack -> Stack)
          | Literal Value

instance Show Expr where
  show (Quote exprs) = show exprs
  show (Word exprs) = show exprs
  show (Literal val) = show val
  show (BuiltinWord _) = "BuiltIn function"

instance Eq Expr where
  (Word expr) == (Word expr') = expr == expr'
  (Quote expr) == (Quote expr') = expr == expr'
  (Literal val) == (Literal val') = val == val'
  _ == _ = False

data Value = VInt Int
           | VQuote [Expr]
           deriving (Show, Eq)

data Stack = Stack [Value]
  deriving (Show, Eq)

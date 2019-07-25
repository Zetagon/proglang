-- | Where the types lie

module Types where

newtype Name = Name String

data Type = ValueType String --ValueName
          | FunctionType [Type] [Type]
          | Unknown

data ValueName = ValueName (Maybe String)

data Expr = Word Name Type
          | Quotation Expr
          | Composition Expr Word

data ExprUnchecked = UWord Name Type
                   | UQuotation ExprUnchecked (Maybe Type)
                   | UList [ExprUnchecked] (Maybe Type)


data Stack a stk = Cons (a, stk)
                    | Nil




eval :: (Stack a stk) -> Expr -> Stack b stk'
eval stack expr = undefined

inferTypes :: ExprUnchecked -> ExprUnchecked
inferTypes = undefined

example =
  UList [ UWord (Name "1") Unknown
        , UWord (Name "2") Unknown
        , UWord (Name "add") (FunctionType [ValueType "Int", ValueType "Int"] [ValueType "Int"])]

-- |

module Level1.Eval (eval, vpop, vdup, vswap, vcat, vcons, vunit, vi, vdip) where

import Level1.Types

eval :: Stack -> [Expr] -> Stack
eval s ((Word (e:exprs)): exprs') = eval ( eval (eval s [e]) exprs ) exprs'
eval (Stack s) ((Quote exprs): exprs') = eval (Stack $ (VQuote exprs):s) exprs'
eval s (BuiltinWord f : exprs) = eval (f s ) exprs
eval (Stack s) ((Literal val):exprs) = eval (Stack $ val:s) exprs
eval s [] = s

e =
  [ Literal $ VInt 2
  , Literal $ VInt 5
  , Literal $ VInt 2
  , vdup
  , BuiltinWord (\(Stack ((VInt x):(VInt y):xs)) -> Stack $ (VInt $ x + y):xs)
  , BuiltinWord (\(Stack ((VInt x):(VInt y):xs)) -> Stack $ (VInt $ x - y):xs)]

  --Builtins
pop :: Stack -> Stack
pop (Stack (x:xs)) = Stack xs
vpop = BuiltinWord pop

dup :: Stack -> Stack
dup (Stack (x:xs)) = Stack (x:x:xs)
vdup = BuiltinWord dup

swap :: Stack -> Stack
swap (Stack (x:y:xs)) = Stack $ y:x:xs
vswap = BuiltinWord swap

cat :: Stack -> Stack
cat (Stack ((VQuote exprs):(VQuote exprs'):xs)) =
  Stack $ (VQuote $ exprs ++ exprs'):xs
vcat = BuiltinWord cat

cons :: Stack -> Stack
cons (Stack ((VQuote exprs):(VQuote exprs'):xs)) =
  Stack $ (VQuote $ (Quote exprs) : exprs'):xs
vcons = BuiltinWord cons

unit :: Stack -> Stack
unit (Stack ((VQuote exprs):xs)) =
  Stack $ ( VQuote $ [Quote exprs]):xs
vunit = BuiltinWord unit

i :: Stack -> Stack
i (Stack ((VQuote exprs):xs)) =
  eval (Stack xs) exprs
vi = BuiltinWord i

dip :: Stack -> Stack
dip (Stack ((VQuote exprs):x:xs)) =
  let Stack s = eval (Stack xs) exprs
  in Stack $ x:s
vdip = BuiltinWord dip

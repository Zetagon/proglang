-- |

module Level1.Eval (eval, vpop, vdup, vswap, vcat, vcons, vunit, vi, vdip) where

import Level1.Types
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict



push  :: Expr -> EvalStateM ()
push x = do
  state <- get
  put $ state { _evalSStack = x:(_evalSStack state) }

pop  :: EvalStateM Expr
pop = do
  state <- get
  let x = head $ _evalSStack state
  put state { _evalSStack = tail $ _evalSStack state}
  return x

peep :: EvalStateM Expr
peep = head <$> _evalSStack <$> get

getWord :: FNName -> EvalStateM [Expr]
getWord name =
  do
    w <- M.lookup <$> pure name <*> (_evalSSEnv <$> get)
    case w of
      Nothing -> error "Word is not defined!"
      Just w -> return w

eval  :: [Expr] -> EvalStateM ()
eval [] = return ()
eval (x:xs) =
  case x of
    Word name -> do eval <$> getWord name
                    eval xs
    q@(Quote _) -> push q
    BuiltinWord fn -> fn
    val@(Literal _) -> push val





  --Builtins
vpop = BuiltinWord $ pop *> pure ()

vdup = BuiltinWord $ peep >>= push

vswap = BuiltinWord $ do
  x <- pop
  y <- pop
  push x
  push y

vcat = BuiltinWord $ do
  x <- pop
  y <- pop
  case (x, y) of
    (Quote exprs, Quote exprs') -> push $ Quote $ exprs ++ exprs'

vcons = BuiltinWord $ do
  x <- pop
  y <- pop
  case (x, y) of
    (Quote exprs, Quote exprs') -> push $ Quote $ (Quote exprs): exprs'

vunit = BuiltinWord $ do
  push =<< Quote <$> (:[]) <$> pop

vi = BuiltinWord $ do
  x <- pop
  case x of
    Quote exprs -> eval exprs
    exprs -> eval [exprs]

vdip = BuiltinWord $ do
  x <- pop
  y <- pop
  push x
  eval [vi]
  push y

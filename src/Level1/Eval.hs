-- |

module Level1.Eval (eval, vzap, vdup, vswap, vcat, vcons, vunit, vi, vdip, push, pop, peep, getProgramEnv) where

import Level1.Types
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict



push  :: Expr -> EvalStateM ()
push x = do
  -- s <- get
  -- put $ s { _evalSStack = x:(_evalSStack s) }
  modify (\s -> s { _evalSStack = x : (_evalSStack s)})

pop  :: EvalStateM Expr
pop = do
  s <- get
  let x = head $ _evalSStack s
  modify (\s' -> s' { _evalSStack = tail $ _evalSStack s'})
  return x

peep :: EvalStateM Expr
peep = head <$> _evalSStack <$> get

getWord :: FNName -> EvalStateM [Expr]
getWord name =
  do
    w <- M.lookup <$> pure name <*> (_evalSSEnv <$> get)
    case w of
      Nothing -> error "Word is not defined!"
      Just expr -> return expr

getProgramEnv :: EvalStateM (M.Map FNName [Expr])
getProgramEnv = _evalSSEnv <$> get

eval  :: [Expr] -> EvalStateM ()
eval [] = return ()
eval (x:xs) =
  case x of
    Word name -> do _ <- eval =<< getWord name
                    eval xs
    q@(Quote _) -> push q >> eval xs
    q@(NewStackQuote _ _) -> push q >> eval xs
    BuiltinWord fn -> fn >> eval xs
    val@(Literal _) -> push val >> eval xs





  --Builtins
vzap = BuiltinWord $ do-- pop *> pure ()
  pop
  return ()

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
    (expr, expr') -> push $ Quote [expr, expr']

vcons = BuiltinWord $ do
  x <- pop
  y <- pop
  case (x, y) of
    (Quote exprs, Quote exprs') -> push $ Quote $ (Quote exprs): exprs'
    (expr, expr') -> push $ Quote [Quote [expr], expr']

vunit = BuiltinWord $ do
  push =<< Quote <$> (:[]) <$> pop


vi = BuiltinWord $ do
  x <- pop
  case x of
    Quote exprs -> eval exprs
    NewStackQuote num exprs ->
      do
        env <- getProgramEnv
        newStack <- reverse <$> take num <$> _evalSStack <$> get
        resStack <-  lift $ _evalSStack <$> execStateT (eval exprs) (EvalState newStack env)
        modify (\oldState ->
                   oldState
                   { _evalSStack = resStack ++ (drop num $ _evalSStack oldState )})
    exprs -> eval [exprs]

vdip = BuiltinWord $ do
  x <- pop
  y <- pop
  push x
  eval [vi]
  push y

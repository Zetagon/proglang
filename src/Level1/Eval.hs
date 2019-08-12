-- |

module Level1.Eval (eval, vzap, vdup, vswap, vcat, vcons, vunit, vi, vdip, push, pop, peep, getProgramEnv) where

import Level1.Types
import qualified Data.Map.Strict as M
import Control.Exception
import Level1.EvalState
import Level1.Errors

eval :: [Expr] -> EvalStateM ()
eval [] = return ()
eval (x:xs) =
  case x of
    Word name -> do _ <- withNewScope $ eval =<< getWord name
                    eval xs
    q@(Quote _) -> push q >> eval xs
    q@(NewStackQuote _ _) -> push q >> eval xs
    BuiltinWord fn -> fn >> eval xs
    val@(Literal _) -> push val >> eval xs
    (Record r) -> push (Record r) >> eval xs
    (AccessField f) -> do r <- pop
                          case r of
                            Record r' -> do
                              let x = (r' M.!? f)
                              case x of
                                Just x' -> push x'
                                Nothing -> runTimeError AccessUnavailableFieldError
                          eval xs
    (UpdateRecord f) -> do val <- pop
                           r <- pop
                           case r of
                             Record r' -> do
                               push $ Record $ M.insert f val r'
    (BindName f) -> do
      x' <- pop
      addWord f x'
      eval xs




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
    (Quote program, arg) -> push $ Quote $ arg:program
    (expr, expr') -> push $ Quote [Quote [expr], expr']

vunit = BuiltinWord $ do
  push =<< Quote <$> (:[]) <$> pop


vi = BuiltinWord $ do
  x <- pop
  case x of
    Quote exprs -> withNewScope $ eval exprs
    NewStackQuote num exprs ->
      do
        newStack <- reverse <$> take num <$> getProgramStack
        newState <- (newStateWithStack newStack)
        resStack <-  _evalSStack <$> execEvalStateM (eval exprs) newState
        modifyProgramStack
          (\oldStack ->
               resStack ++ (drop num $ oldStack ))

    exprs -> eval [exprs]

vdip = BuiltinWord $ do
  x <- pop
  y <- pop
  push x
  eval [vi]
  push y

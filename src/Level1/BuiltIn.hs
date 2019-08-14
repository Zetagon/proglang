-- |

module Level1.BuiltIn where

import Level1.Types
import Level1.Eval
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict

-- | Run an interpreter with the defualt environment
runDefaultEvalStateM :: EvalStateM a -> IO EvalState
runDefaultEvalStateM m = runEvalStateM m defaultEnv

-- | The default environment
defaultEnv :: M.Map FNName [Expr]
defaultEnv = M.fromList
  [ (FNName "if", [ vi ])
  , (FNName "true", [ Quote [ vzap
                            , vi] ])
  , (FNName "false", [ Quote [ Quote [vzap]
                             , vdip
                             , vi]])
  , (FNName "join2", [ vunit
                     , vswap
                     , vunit
                     , vswap
                     , vcat])
  , (FNName "rot3", [ Quote [vswap]
                    , vdip
                    , vswap])
  , (FNName "rot3b", [ Word $ FNName "rot3"
                     , Word $ FNName "rot3"])
  , (FNName "fib", [ Word $ FNName "rot3" -- get i
                   , vdup
                   , Quote [ Quote [ vswap
                                   , Quote [ vdup ]
                                   , vdip
                                   , add -- add fib here
                                   , Word $ FNName "fib"
                                   ]]
                   , Quote [Quote [Word $ FNName "rot3", vzap, vswap, vzap]]
                   , Word $ FNName "rot3"
                   , Literal $ VInt 0
                   , largerThan
                   , vif
                   , Quote [Literal $ VInt 1, vswap, sub]
                   , vdip
                   , Quote [Word $ FNName "rot3b"]
                   , vdip
                   , vi
                   ])
  , (FNName "group3", [Quote [ vunit
                             , vswap
                             , vunit
                             , vswap
                             , Word $ FNName "rot3"
                             , vunit
                             , Word $ FNName "rot3b"
                             , vcat
                             , vcat]])
  , (FNName "cons3", [ Word $ FNName "group3"
                     , vdip
                     , vcons])
  , (FNName "fib2", [
        Word $ FNName "rot3"
        , vdup
        , Quote [ Word $ FNName "rot3b" ], vdip
        , Word $ FNName "rot3b" -- make 2 is at the bottom of stack
        , Word $ FNName "group3"
        , vdup
        , Quote [ Quote [ vdup ]
                , vdip
                , add
                , Word $ FNName "fib2"]
        , vcons
        , vswap
        , Quote [ Quote [ vzap ]
                , vdip
                , Quote [ vzap ]
                , vdip
                ]
        , vcons -- i [true] [ false ]
        , Word $ FNName "rot3"
        , Literal $ VInt $ 0
        , largerThan
        , vif
        ])
    , (FNName "fib3", [
                        vunit
                      , BindName $ FNName "a"
                      , vunit
                      , BindName $ FNName "b"
                      , vunit
                      , BindName $ FNName "i"

                      , Quote [
                                Literal $ VInt 1
                              , Word $ FNName "i"
                              , sub
                              , Word $ FNName "a"
                              , Word $ FNName "b"
                              , Word $ FNName "a"
                              , add
                              , Word $ FNName "fib3"]
                      , Quote [ Word $ FNName "b"
                              -- , BuiltinWord $ do
                              --     env <- getProgramEnv
                              --     lift $ print env
                              --     a <- (getWord $ FNName "a")
                              --     b <- (getWord $ FNName "b")
                              --     i <- (getWord $ FNName "i")
                              --     lift $ print a
                              --     lift $ print b
                              --     lift $ print i
                              --     stack <- getProgramStack
                              --     lift $ print stack
                              ]
                      , Word $ FNName "i"
                      , Literal $ VInt 0
                      , BuiltinWord $ do
                          -- env <- getProgramEnv
                          -- lift $ print env
                          -- a <- (getWord $ FNName "a")
                          b <- (getWord $ FNName "b")
                          -- i <- (getWord $ FNName "i")
                          -- lift $ print a
                          lift $ print b
                          -- lift $ print i
                          -- stack <- getProgramStack
                          -- lift $ print stack
                      , largerThan
                      , vif])]

  -- i
-- a
-- b

-- rot3 dup t f
-- a
-- b
-- i
-- i
-- t
-- f

-- rot3
-- a
-- b
-- i
-- t
-- f
-- i

-- a
-- b
-- i
-- t
-- f
-- i
-- 0

-- largerThan
-- a
-- b
-- i
-- t
-- f
-- boolean

-- if
-- a
-- b
-- i
-- branch


-- [rot3b] dip
-- i
-- a
-- b
-- branch




-- ----------

-- i
-- a
-- b

-- swap
-- i
-- b
-- a

-- [dup] dip
-- i
-- b
-- b
-- a

-- add
-- i
-- b
-- b + a

add :: Expr
add = BuiltinWord $ do
  x <- pop
  y <- pop
  case (x,y) of
    (Literal (VInt x'), Literal (VInt y')) -> push (Literal $ VInt $ x' + y')

sub = BuiltinWord $ do
  x <- pop
  y <- pop
  case (x,y) of
    (Literal (VInt x'), Literal (VInt y')) -> push (Literal $ VInt $ x' - y')

largerThan = BuiltinWord $ do
  y <- pop
  x <- pop
  case (x, y) of
    (Literal (VInt x'), Literal (VInt y')) ->
      -- push =<< (if x' > y'
                -- then head <$> getWord $  FNName "true"
                -- else head <$> getWord $  FNName "false")
      eval =<< ( if x' > y'
                 then getWord $ FNName "true"
                 else getWord $ FNName "false")
    (x', y') -> do
      state <- get
      error ("Expected two numbers. Got instead:\n x: " ++
              show x' ++
              "\n y: " ++ show y' ++
              "\n The state looked like this:\n" ++
              show (state :: EvalState))

vif :: Expr
vif = Word $ FNName "if"

vtrue :: Expr
vtrue = Word $ FNName "true"

vfalse :: Expr
vfalse = Word $ FNName "false"

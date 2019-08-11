-- |
--

module Level1.EvalState
  ( lift
  , modify
  , execStateT
  , EvalStateM, newStateWithStack, runEvalStateM, execEvalStateM, EvalState, _evalSStack, push, pop, peep, getWord, getProgramEnv, getProgramStack, modifyProgramStack, runTimeError, RecordMap, FieldName(..), Expr(..), FNName(..), Value(..))
where

import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad.State.Strict

-- class EvalStateM where
--   push :: Expr -> EvalStateM ()
--   pop  :: EvalStateM Expr
--   peep :: EvalStateM Expr
--   getWord :: FNName -> EvalStateM [Expr]
--   eval  :: [Expr] -> EvalStateM ()



data EvalState = EvalState
                    { _evalSStack :: ![Expr]
                    , _evalSSEnv :: Environment }
  deriving (Show)

data Environment = Environment ![M.Map FNName [Expr]]
  deriving (Show)

type EvalStateM a = StateT EvalState IO a

execEvalStateM :: EvalStateM a -> EvalState -> EvalStateM EvalState
execEvalStateM m s = lift $ execStateT m s


runEvalStateM :: EvalStateM a -> M.Map FNName [Expr] -> IO EvalState
runEvalStateM m env = execStateT m (EvalState [] (Environment [env]) )

newStateWithStack :: [Expr] -> EvalStateM (EvalState)
newStateWithStack stack = do
  env <- getProgramEnv
  return $ EvalState stack env



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
    w <- lookThroughEnv' <$> (_evalSSEnv <$> get)
    case w of
      Nothing -> error "Word is not defined!"
      Just expr -> return expr
      where
        lookThroughEnv' (Environment xs) = lookThroughEnv xs
        lookThroughEnv :: [M.Map FNName v] -> Maybe v
        lookThroughEnv [] = Nothing
        lookThroughEnv (x:xs) =
          case M.lookup name x of
            Nothing -> lookThroughEnv xs
            Just v -> Just v

getProgramEnv :: EvalStateM (Environment)
getProgramEnv = _evalSSEnv <$> get

getProgramStack :: EvalStateM [Expr]
getProgramStack = _evalSStack <$> get

modifyProgramStack :: ([Expr] -> [Expr]) -> EvalStateM ()
modifyProgramStack f = modify (\oldState ->
                                 oldState
                                { _evalSStack = f $ _evalSStack oldState })


runTimeError :: (Exception a) => a -> EvalStateM ()
runTimeError = lift . throwIO

type RecordMap = M.Map FieldName Expr
newtype FieldName = FieldName String
  deriving (Eq, Ord, Show)

data Expr = Word FNName
            | Quote [Expr]
            | NewStackQuote Int [Expr]
            | BuiltinWord (EvalStateM ())
            | Literal Value
            | Record  RecordMap
            | AccessField FieldName
            | UpdateRecord FieldName

instance Show Expr where
  show (Quote exprs) = "Quote: " ++ show exprs
  show (Word exprs) = "Word: " ++ show exprs
  show (Literal val) = show val
  show (BuiltinWord _) = "BuiltIn function"
  show (Record r) = "Record " ++ show r
  show (AccessField f) = "AccesField " ++ show f

instance Eq Expr where
  (Word expr) == (Word expr') = expr == expr'
  (Quote expr) == (Quote expr') = expr == expr'
  (Literal val) == (Literal val') = val == val'
  _ == _ = False

data Value = VInt !Int
           | VQuote [Expr]
           | VRecord RecordMap
           deriving (Show, Eq)

newtype FNName = FNName String
  deriving (Eq, Show, Ord)

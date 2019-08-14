-- |
--

module Level1.EvalState
  ( lift
  , modify
  , execStateT
  , EvalStateM, withNewScope, newStateWithStack,evalEvalStateM, runEvalStateM, execEvalStateM, EvalState, _evalSSEnv, _evalSStack, push, pop, peep, addWord, getWord, getProgramEnv, getProgramStack, modifyProgramStack, runTimeError, RecordMap, FieldName(..), Expr(..), FNName(..), Value(..))
where

import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad.State.Strict
import Level1.Errors

-- class EvalStateM where
--   push :: Expr -> EvalStateM ()
--   pop  :: EvalStateM Expr
--   peep :: EvalStateM Expr
--   getWord :: FNName -> EvalStateM [Expr]
--   eval  :: [Expr] -> EvalStateM ()



-- | The state of the interpreter
data EvalState = EvalState
                    { _evalSStack :: ![Expr] -- ^ The program stack where all the values are stored
                    , _evalSSEnv :: Environment -- ^ The program environment. See 'Environment'
                    }
  deriving (Show)

-- | The program environment where all the top level defintions are stored. Later name bindings are store here with lexical scoping
data Environment = Environment ![M.Map FNName [Expr]]
  deriving (Show)

-- | The interpreter monad
type EvalStateM a = (StateT EvalState IO a)

-- | Run the interpreter and return the final state.
execEvalStateM :: EvalStateM a -> EvalState -> EvalStateM EvalState
execEvalStateM m s = lift $ execStateT m s


-- | run the interpreter with given environment
runEvalStateM :: EvalStateM a -> M.Map FNName [Expr] -> IO EvalState
runEvalStateM m env = execStateT m (EvalState [] (Environment [env]) )

-- | run the interpreter with given environment.
-- Return the final result of the monad
evalEvalStateM :: EvalStateM a -> M.Map FNName [Expr] -> IO a
evalEvalStateM m env = evalStateT m (EvalState [] (Environment [env]) )

-- | TODO
newStateWithStack :: [Expr] -> EvalStateM (EvalState)
newStateWithStack stack = do
  env <- getProgramEnv
  return $ EvalState stack env

-- | run m with a new scope inserted
withNewScope :: EvalStateM a -- ^ m
  -> EvalStateM ()
withNewScope m = do
  s <- get
  Environment env <- getProgramEnv
  newState <- execEvalStateM m $ s { _evalSSEnv = Environment $ M.empty : env }
  modify (\oldState -> oldState { _evalSStack = _evalSStack newState })
  return ()


-- | push a new value onto the program stack
push  :: Expr -> EvalStateM ()
push x = do
  -- s <- get
  -- put $ s { _evalSStack = x:(_evalSStack s) }
  modify (\s -> s { _evalSStack = x : (_evalSStack s)})


-- | pop a value from the program stack
pop  :: EvalStateM Expr
pop = do
  s <- get
  let x = head $ _evalSStack s
  modify (\s' -> s' { _evalSStack = tail $ _evalSStack s'})
  return x

-- | get the top value from the program stack
peep :: EvalStateM Expr
peep = head <$> _evalSStack <$> get

-- Add a word to the program environment with the given name
addWord :: FNName -> Expr -> EvalStateM ()
addWord name (Quote exprs) = do
  modify (\s -> s {
             _evalSSEnv = let Environment (x:xs) = _evalSSEnv s
                              x' =  M.insert name exprs x
                          in Environment (x':xs)})

-- Get the word with the given name from the environment
getWord :: FNName -> EvalStateM [Expr]
getWord name =
  do
    w <- lookThroughEnv' <$> (_evalSSEnv <$> get)
    case w of
      Nothing -> throw WordIsNotDefinedError
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

-- | The Ast
data Expr = Word FNName -- ^ a function
            | Quote [Expr] -- ^ Unevaluated ast
            | NewStackQuote Int [Expr] -- ^ TODO
            | BuiltinWord (EvalStateM ()) -- ^ A word that is defined in haskell code
            | Literal Value -- ^ A value
            | Record  RecordMap -- ^ A record
            | AccessField FieldName -- ^ Get the word with the name from the record ontop of the stack and push it onto the stack
            | UpdateRecord FieldName -- ^ Update a field from the record at the top of the stack
            | BindName FNName -- ^ Bind the item on the top of the stack to a name

instance Show Expr where
  show (Quote exprs) = "Quote: " ++ show exprs
  show (Word exprs) = "Word: " ++ show exprs
  show (Literal val) = show val
  show (BuiltinWord _) = "BuiltIn function"
  show (Record r) = "Record " ++ show r
  show (AccessField f) = "AccesField " ++ show f
  show (BindName n) = "BindName " ++ show n

instance Eq Expr where
  (Word expr) == (Word expr') = expr == expr'
  (Quote expr) == (Quote expr') = expr == expr'
  (Literal val) == (Literal val') = val == val'
  _ == _ = False

-- The base values in the language
data Value = VInt !Int
           | VQuote [Expr]
           | VRecord RecordMap
           deriving (Show, Eq)

-- Names for functions
newtype FNName = FNName String
  deriving (Eq, Show, Ord)

-- | Untyped Language

module Level1.Types
  ( module Level1.EvalState
  , module Level1.Types)
where

import qualified Data.Map.Strict as M
import Control.Exception
import Control.Monad.State.Strict
import Level1.EvalState

-- | The compiler did something wrong and our assumptions have been violated
data CompilerError =
  AccessUnavailableFieldError
  deriving (Show, Eq)

instance Exception CompilerError

-- |

module AdvancedCode where
import Level1.Eval
import Level1.Types
import Level1.BuiltIn
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Test.Hspec

advanced = do
  describe "Code Examples" $ do
    it "can run fibonacci" $ do
      _evalSStack <$> execStateT (eval [ Literal $ VInt 4
                                       , Literal $ VInt 1
                                       , Literal $ VInt 1
                                       , Word $ FNName "fib"])
        (EvalState [] defaultEnv) `shouldReturn` [Literal $ VInt 2]

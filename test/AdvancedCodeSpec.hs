-- |

module AdvancedCodeSpec where
import Level1.Eval
import Level1.Types
import Level1.BuiltIn
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Test.Hspec

spec :: Spec
spec = do
  describe "Code Examples" $ do
    it "can run fibonacci" $ do
      _evalSStack <$> runDefaultEvalStateM (eval [ Literal $ VInt 4
                                                 , Literal $ VInt 1
                                                 , Literal $ VInt 1
                                                 , Word $ FNName "fib"])
         `shouldReturn` [Literal $ VInt 8]

    -- it "can run fibonacci2" $ do
    --   _evalSStack <$> runDefaultEvalStateM (eval [ Literal $ VInt 4
    --                                              , Literal $ VInt 1
    --                                              , Literal $ VInt 1
    --                                              , Word $ FNName "fib2"])
    --      `shouldReturn` [Literal $ VInt 2]

    it "can run fibonacci3" $ do
      _evalSStack <$> runDefaultEvalStateM (eval [ Literal $ VInt 5
                                                 , Literal $ VInt 1
                                                 , Literal $ VInt 1
                                                 , Word $ FNName "fib3"])
         `shouldReturn` [Literal $ VInt 8]

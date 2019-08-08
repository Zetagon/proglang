-- |

module Level1.EvalStateSpec where
import Test.Hspec
import Test.Hspec.Runner
import qualified Data.Map.Strict as M
import Level1.EvalState

spec :: Spec
spec = do
    describe "EvalStateM" $ do
      describe "push" $ do
        it "adds 1 element to the stack" $ do
          ( _evalSStack <$> execStateT (push $ Literal $ VInt 1) (EvalState [] M.empty)
            `shouldReturn` ([ Literal $ VInt 1]))

        it "adds 2 element to the stack" $ do
          expected <- execStateT (do
                                     push $ Literal $ VInt 1
                                     push $ Literal $ VInt 2
                                 ) (EvalState [] M.empty)
          _evalSStack expected `shouldBe` ([Literal $ VInt 2, Literal $ VInt 1])

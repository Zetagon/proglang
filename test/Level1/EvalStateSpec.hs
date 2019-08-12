-- |

module Level1.EvalStateSpec where
import Test.Hspec
import Test.Hspec.Runner
import qualified Data.Map.Strict as M
import Level1.EvalState
import Level1.BuiltIn

spec :: Spec
spec = do
    describe "EvalStateM" $ do
      describe "push" $ do
        it "adds 1 element to the stack" $ do
          ( _evalSStack <$> runDefaultEvalStateM (push $ Literal $ VInt 1)
            `shouldReturn` ([ Literal $ VInt 1]))

        it "adds 2 element to the stack" $ do
          expected <- runDefaultEvalStateM (do
                                     push $ Literal $ VInt 1
                                     push $ Literal $ VInt 2
                                 )
          _evalSStack expected `shouldBe` ([Literal $ VInt 2, Literal $ VInt 1])


      describe "addWord" $ do
        it "can add a word to env" $ do
          expected <-  evalEvalStateM (do addWord (FNName "foo") $ Quote [ Literal $ VInt 3 ]
                                          getWord (FNName "foo"))
                       (defaultEnv)
          (expected) `shouldBe` ([Literal $ VInt 3])

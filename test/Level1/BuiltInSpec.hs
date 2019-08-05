-- |

module Level1.BuiltInSpec where
import Test.Hspec

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Level1.Eval
import Level1.Types
import Level1.BuiltIn

spec :: Spec
spec = do
  describe "BuiltIn" $ do
      it "can add three numbers on the stack" $ do
        expected <- execStateT (eval [ Literal $ VInt 1
                                     , Literal $ VInt 2
                                     , Literal $ VInt 3
                                     , add
                                     , add])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 6])

      it "can subtract three numbers on the stack" $ do
        expected <- execStateT (eval [ Literal $ VInt 1
                                     , Literal $ VInt 2
                                     , Literal $ VInt 3
                                     , sub
                                     , sub])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt ((3 - 2) - 1)])
      it "can eval the true branch of an if expr" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 1
                                         , Literal $ VInt 0
                                         , vtrue
                                         , vif])
          (EvalState [] defaultEnv) `shouldReturn` [Literal $ VInt 1]

      it "can eval the false branch of an if expr" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 1
                                         , Literal $ VInt 0
                                         , vfalse
                                         , vif])
          (EvalState [] defaultEnv) `shouldReturn` [Literal $ VInt 0]

      it "can eval join2" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 1
                                         , Literal $ VInt 3
                                         , Word $ FNName "join2"])
          (EvalState [] defaultEnv) `shouldReturn` [ Quote [ Literal $ VInt 3
                                                           , Literal $ VInt 1]]

      it "can eval rot3" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 1
                                         , Literal $ VInt 2
                                         , Literal $ VInt 3
                                         , Word $ FNName "rot3"])
          (EvalState [] defaultEnv) `shouldReturn` [ Literal $ VInt 1
                                                   , Literal $ VInt 3
                                                   , Literal $ VInt 2]

      it "can eval rot3b" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 1
                                         , Literal $ VInt 2
                                         , Literal $ VInt 3
                                         , Word $ FNName "rot3b"])
          (EvalState [] defaultEnv) `shouldReturn` [ Literal $ VInt 2 -- 3
                                                   , Literal $ VInt 1 -- 2
                                                   , Literal $ VInt 3]-- 1

      it "can eval the larger than" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 2
                                         , Literal $ VInt 4
                                         , Literal $ VInt 1
                                         , Literal $ VInt 0
                                         , largerThan
                                         , vif])
          (EvalState [] defaultEnv) `shouldReturn` [Literal $ VInt 2]

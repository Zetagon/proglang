-- |

module Level1.EvalSpec where
import Test.Hspec

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Level1.Eval
import Level1.Types
import Level1.BuiltIn

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
      it "adds ints to the stack" $ do
        expected <- execStateT (eval [ Literal $ VInt 1
                                     , Literal $ VInt 2
                                     , Literal $ VInt 3])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([ Literal $ VInt 3
                                         , Literal $ VInt 2
                                         , Literal $ VInt 1])

      it "can add a record to the stack" $ do
        expected <-  execStateT (eval [Record (M.singleton (FieldName "foo")
                                                  (Literal $ VInt 42))])
                     (EvalState [] M.empty)
        let k = case  head $ _evalSStack $ expected of
              Record r ->   M.lookup (FieldName "foo") r
        k `shouldBe` (Just $ Literal $ VInt 42)

      it "can extract a field from a record" $ do
        _evalSStack <$> execStateT (eval [ Record (M.singleton (FieldName "foo")
                                                   (Literal $ VInt 42))
                                         , AccessField $ FieldName "foo"])
          (EvalState [] M.empty)
        `shouldReturn` [Literal $ VInt 42]

      it "errors when accessing a non-existing field" $ do
        _evalSStack <$> execStateT (eval [ Record (M.singleton (FieldName "foo")
                                                   (Literal $ VInt 42))
                                         , AccessField $ FieldName "bar"])
          (EvalState [] M.empty)
        `shouldThrow` anyException

      it "can eval zap" $ do
        expected <- execStateT (eval [Literal $ VInt 4, vzap])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([])

      it "can eval dup" $ do
        expected <- execStateT (eval [Literal $ VInt 4, vdup])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 4, Literal $ VInt 4])

      it "can swap items on the stack" $ do
        expected <- execStateT (eval [ Literal $ VInt 2
                                     , Literal $ VInt 3
                                     , vswap])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 2, Literal $ VInt 3])

      it "can eval cat for literals" $ do
        expected <- execStateT (eval [Literal $ VInt 4, Literal $ VInt 4, vcat])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Quote [Literal $ VInt 4, Literal $ VInt 4]])

      it "can eval cat for quote" $ do
        expected <- execStateT (eval [Quote [Literal $ VInt 4], Quote [Literal $ VInt 4], vcat])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Quote [Literal $ VInt 4, Literal $ VInt 4]])

      it "can eval cons for literals" $ do
        expected <- execStateT (eval [Literal $ VInt 4, Literal $ VInt 4, vcons])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Quote [Quote [Literal $ VInt 4], Literal $ VInt 4]])

      it "can eval cons for quote" $ do
        expected <- execStateT (eval [Quote [Literal $ VInt 4], Quote [Literal $ VInt 4], vcons])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Quote [Quote [Literal $ VInt 4], Literal $ VInt 4]])
      it "can eval unit" $ do
        expected <- execStateT (eval [Literal $ VInt 4, vunit])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Quote [Literal $ VInt 4]])

      it "can eval i" $ do
        expected <- execStateT (eval [ Quote [ Literal $ VInt 3], vi])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 3])

      it "can use i to apply quote" $ do
        expected <- execStateT (eval [ Literal $ VInt 5
                                     , Quote [ Literal $ VInt 3, add], vi])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 8])
      it "can eval dip" $ do
        expected <- execStateT (eval [ Literal $ VInt 5
                                     , Literal $ VInt 9, Quote [ Literal $ VInt 3, add], vdip])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 9, Literal $ VInt 8])

      it "can eval a function in the environment" $ do
        expected <- execStateT (eval [ Literal $ VInt 3
                                     , Literal $ VInt 4
                                     , Literal $ VInt 5
                                     , Word $ FNName "add3"])
                    (EvalState [] $ M.singleton (FNName "add3") [add, add])

        _evalSStack expected `shouldBe` [Literal $ VInt 12]

      it "errors when evaling NewStackQuote with not enough args" $ do
        execStateT (eval [ Literal $ VInt 1
                                     , NewStackQuote 0 [ Literal $ VInt 1
                                                       , add]
                                     , vi])
                    (EvalState [] M.empty) `shouldThrow` anyException

      it "evals when evaling NewStackQuote with 1 argument" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 1
                                         , NewStackQuote 1 [ Literal $ VInt 1
                                                           , add]
                                     , vi])
                    (EvalState [] M.empty) `shouldReturn` [Literal $ VInt 2]

      it "evals when evaling NewStackQuote with 2 arguments" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 10
                                         , Literal $ VInt 1
                                         , NewStackQuote 2 [add]
                                     , vi])
                    (EvalState [] M.empty) `shouldReturn` [Literal $ VInt 11]

      it "evals when evaling NewStackQuote with 1 argument and a large stack" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 4
                                         , Literal $ VInt 1
                                         , NewStackQuote 1 [ Literal $ VInt 1
                                                           , add]
                                     , vi])
                    (EvalState [] M.empty) `shouldReturn` [Literal $ VInt 2, Literal $ VInt 4]

      it "evals when evaling NewStackQuote" $ do
        _evalSStack <$> execStateT (eval [ Literal $ VInt 1
                                         , NewStackQuote 0 [ Literal $ VInt 1
                                                           , Literal $ VInt 2
                                                           , add]
                                         , vi])
                    (EvalState [] M.empty) `shouldReturn` [Literal $ VInt 3, Literal $ VInt 1]

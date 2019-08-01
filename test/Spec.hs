import Level1.Eval
import Level1.Types
import Level1.BuiltIn
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Test.Hspec

main :: IO ()
main = do
  lv1

lv1 :: IO ()
lv1 = hspec $ do
  describe "Level1" $ do
    describe "EvalStateM" $ do
      describe "push" $ do
        it "adds 1 element to the stack" $ do
          expected <- execStateT (push $ Literal $ VInt 1) (EvalState [] M.empty)
          _evalSStack expected `shouldBe` ([ Literal $ VInt 1])

        it "adds 2 element to the stack" $ do
          expected <- execStateT (do
                                     push $ Literal $ VInt 1
                                     push $ Literal $ VInt 2
                                 ) (EvalState [] M.empty)
          _evalSStack expected `shouldBe` ([Literal $ VInt 2, Literal $ VInt 1])

    describe "basic integration stuff" $ do
      it "adds ints to the stack" $ do
        expected <- execStateT (eval [ Literal $ VInt 1
                                     , Literal $ VInt 2
                                     , Literal $ VInt 3])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([ Literal $ VInt 3
                                         , Literal $ VInt 2
                                         , Literal $ VInt 1])


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

      it "can eval zap" $ do
        expected <- execStateT (eval [Literal $ VInt 4, vzap])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([])

      it "can eval dup" $ do
        expected <- execStateT (eval [Literal $ VInt 4, vdup])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 4, Literal $ VInt 4])

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

      it "can swap items on the stack" $ do
        expected <- execStateT (eval [ Literal $ VInt 2
                                     , Literal $ VInt 3
                                     , vswap])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 2, Literal $ VInt 3])
      it "can use i" $ do
        expected <- execStateT (eval [ Quote [ Literal $ VInt 3], vi])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 3])
      it "can use i to apply quote" $ do
        expected <- execStateT (eval [ Literal $ VInt 5
                                     , Quote [ Literal $ VInt 3, add], vi])
                    (EvalState [] M.empty)
        _evalSStack expected `shouldBe` ([Literal $ VInt 8])



-- tests = test [ "addition" ~:
--              (do
--                let expected = ([Literal $ VInt 6])
--                result <- execStateT (eval
--                                        [ Literal $ VInt 2
--                                        , Literal $ VInt 2
--                                        , Literal $ VInt 2
--                                        , add
--                                        , add])
--                          (EvalState [] M.empty)
--                expected ~=? result
--              )]
--              -- , "subtraction" ~:
--              --   (Stack [VInt 3])
--              --   ~=?
--              --   (eval (Stack [])
--              --    [ Literal $ VInt 2
--              --    , Literal $ VInt 4
--              --    , Literal $ VInt 1
--              --    , add
--              --    , sub]
--              --   )
--              -- , "Quote" ~:
--              --   (Stack [VInt 5])
--              --   ~=?
--              --   (eval (Stack [])
--              --    [ Literal $ VInt 3
--              --    , Literal $ VInt 1
--              --    , Literal $ VInt 1
--              --    , Quote [ add, add ]
--              --    , vi ])
--              -- , "dip" ~:
--              --   (Stack [VInt 1, VInt 4])
--              --   ~=?
--              --   (eval (Stack [])
--              --    [ Literal $ VInt 3
--              --    , Literal $ VInt 1
--              --    , Literal $ VInt 1
--              --    , Quote [ add ]
--              --    , vdip ])]

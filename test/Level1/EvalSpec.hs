-- |

module Level1.EvalSpec where
import Test.Hspec
import Test.Hspec.Runner

import qualified Data.Map.Strict as M
import Level1.Eval
import Level1.Types
import Level1.BuiltIn
import Level1.Errors

spec :: Spec
spec = do
      describe "eval" $ do
        it "adds ints to the stack" $ do
          expected <- runDefaultEvalStateM (eval [ Literal $ VInt 1
                                                 , Literal $ VInt 2
                                                 , Literal $ VInt 3])
          _evalSStack expected `shouldBe` ([ Literal $ VInt 3
                                           , Literal $ VInt 2
                                           , Literal $ VInt 1])

        it "can add a record to the stack" $ do
          expected <-  runDefaultEvalStateM (eval [Record (M.singleton (FieldName "foo")
                                                  (Literal $ VInt 42))])
          let k = case  head $ _evalSStack $ expected of
                Record r ->   M.lookup (FieldName "foo") r
          k `shouldBe` (Just $ Literal $ VInt 42)

        it "can extract a field from a record" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Record (M.singleton (FieldName "foo")
                                                               (Literal $ VInt 42))
                                                     , AccessField $ FieldName "foo"])
            `shouldReturn` [Literal $ VInt 42]

        it "errors when accessing a non-existing field" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Record (M.singleton (FieldName "foo")
                                                               (Literal $ VInt 42))
                                                     , AccessField $ FieldName "bar"])
            `shouldThrow` (== AccessUnavailableFieldError)

        it "can update an existing field" $ do
          expected <- runDefaultEvalStateM (eval [ Record (M.singleton (FieldName "foo")
                                                           (Literal $ VInt 42))
                                                 , Literal $ VInt 4
                                                 , UpdateRecord $ FieldName "foo"])
          let k = case head $ _evalSStack $ expected of
                      Record r -> M.lookup (FieldName "foo") r
          k `shouldBe` (Just $ Literal $ VInt 4)


        it "can eval zap" $ do
          expected <- runDefaultEvalStateM (eval [Literal $ VInt 4, vzap])
          _evalSStack expected `shouldBe` ([])

        it "can eval dup" $ do
          expected <- runDefaultEvalStateM (eval [Literal $ VInt 4, vdup])
          _evalSStack expected `shouldBe` ([Literal $ VInt 4, Literal $ VInt 4])

        it "can swap items on the stack" $ do
          expected <- runDefaultEvalStateM (eval [ Literal $ VInt 2
                                                 , Literal $ VInt 3
                                                 , vswap])
          _evalSStack expected `shouldBe` ([Literal $ VInt 2, Literal $ VInt 3])

        it "can eval cat for literals" $ do
          expected <- runDefaultEvalStateM (eval [Literal $ VInt 4, Literal $ VInt 4, vcat])
          _evalSStack expected `shouldBe` ([Quote [Literal $ VInt 4, Literal $ VInt 4]])

        it "can eval cat for quote" $ do
          expected <- runDefaultEvalStateM (eval [Quote [Literal $ VInt 4], Quote [Literal $ VInt 4], vcat])
          _evalSStack expected `shouldBe` ([Quote [Literal $ VInt 4, Literal $ VInt 4]])

        it "can hardwire parameter with cons" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Literal $ VInt 4
                                                     , Quote [ Literal $ VInt 3
                                                             , add]
                                                     , vcons
                                                     , vi
                                                     ])
            `shouldReturn` [ Literal $ VInt 7 ]


        it "can eval cons for literals" $ do
          expected <- runDefaultEvalStateM (eval [Literal $ VInt 4, Literal $ VInt 4, vcons])
          _evalSStack expected `shouldBe` ([Quote [Quote [Literal $ VInt 4], Literal $ VInt 4]])

        it "can eval cons for quote" $ do
          expected <- runDefaultEvalStateM (eval [Quote [Literal $ VInt 4], Quote [Literal $ VInt 4], vcons])
          _evalSStack expected `shouldBe` ([Quote [Quote [Literal $ VInt 4], Literal $ VInt 4]])
        it "can eval unit" $ do
          expected <- runDefaultEvalStateM (eval [Literal $ VInt 4, vunit])
          _evalSStack expected `shouldBe` ([Quote [Literal $ VInt 4]])

        it "can eval i" $ do
          expected <- runDefaultEvalStateM (eval [ Quote [ Literal $ VInt 3], vi])
          _evalSStack expected `shouldBe` ([Literal $ VInt 3])

        it "can use i to apply quote" $ do
          expected <- runDefaultEvalStateM (eval [ Literal $ VInt 5
                                                 , Quote [ Literal $ VInt 3, add], vi])
          _evalSStack expected `shouldBe` ([Literal $ VInt 8])
        it "can eval dip" $ do
          expected <- runDefaultEvalStateM (eval [ Literal $ VInt 5
                                                 , Literal $ VInt 9, Quote [ Literal $ VInt 3, add], vdip])
          _evalSStack expected `shouldBe` ([Literal $ VInt 9, Literal $ VInt 8])

        it "can eval a function in the environment" $ do
          expected <- runEvalStateM (eval [ Literal $ VInt 3
                                          , Literal $ VInt 4
                                          , Literal $ VInt 5
                                          , Word $ FNName "add3"])
                      ( M.singleton (FNName "add3") [add, add])

          _evalSStack expected `shouldBe` [Literal $ VInt 12]

        it "errors when evaling NewStackQuote with not enough args" $ do
          runDefaultEvalStateM (eval [ Literal $ VInt 1
                                     , NewStackQuote 0 [ Literal $ VInt 1
                                                       , add]
                                     , vi])
             `shouldThrow` anyException

        it "evals when evaling NewStackQuote with 1 argument" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Literal $ VInt 1
                                                     , NewStackQuote 1 [ Literal $ VInt 1
                                                                       , add]
                                                     , vi])
             `shouldReturn` [Literal $ VInt 2]

        it "evals when evaling NewStackQuote with 2 arguments" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Literal $ VInt 10
                                                     , Literal $ VInt 1
                                                     , NewStackQuote 2 [add]
                                                     , vi])
             `shouldReturn` [Literal $ VInt 11]

        it "evals when evaling NewStackQuote with 1 argument and a large stack" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Literal $ VInt 4
                                                     , Literal $ VInt 1
                                                     , NewStackQuote 1 [ Literal $ VInt 1
                                                                       , add]
                                                     , vi])
             `shouldReturn` [Literal $ VInt 2, Literal $ VInt 4]

        it "evals when evaling NewStackQuote" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Literal $ VInt 1
                                                     , NewStackQuote 0 [ Literal $ VInt 1
                                                                       , Literal $ VInt 2
                                                                       , add]
                                                     , vi])
             `shouldReturn` [Literal $ VInt 3, Literal $ VInt 1]

        it "can bind a function and call it" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Quote [ add ]
                                                     , BindName $ FNName "foo"
                                                     , Literal $ VInt 4
                                                     , Literal $ VInt 3
                                                     , Word $ FNName "foo"])
            `shouldReturn` [Literal $ VInt 7]

        it "can bind a function in a scope and call it" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Quote [ Quote [ add ]
                                                             , BindName $ FNName "foo"
                                                             , Literal $ VInt 4
                                                             , Literal $ VInt 3
                                                             , Word $ FNName "foo"]
                                                     , vi])
            `shouldReturn` [Literal $ VInt 7]


        it "can't call a word out of scope" $ do
          _evalSStack <$> runDefaultEvalStateM (eval [ Quote [ Quote [ add ]
                                                             , BindName $ FNName "foo"]
                                                     , vi
                                                     , Literal $ VInt 4
                                                     , Literal $ VInt 3
                                                     , Word $ FNName "foo"])
            `shouldThrow` (== WordIsNotDefinedError)

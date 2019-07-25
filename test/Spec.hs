import Level1.Eval
import Level1.Types
import Level1.BuiltIn
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

tests = test [ "addition" ~:
               (Stack [VInt 6])
               ~=?
               (eval (Stack [])
                [ Literal $ VInt 2
                , Literal $ VInt 2
                , Literal $ VInt 2
                , add
                , add])
             , "subtraction" ~:
               (Stack [VInt 3])
               ~=?
               (eval (Stack [])
                [ Literal $ VInt 2
                , Literal $ VInt 4
                , Literal $ VInt 1
                , add
                , sub]
               )
             , "Quote" ~:
               (Stack [VInt 5])
               ~=?
               (eval (Stack [])
                [ Literal $ VInt 3
                , Literal $ VInt 1
                , Literal $ VInt 1
                , Quote [ add, add ]
                , vi ])
             , "dip" ~:
               (Stack [VInt 1, VInt 4])
               ~=?
               (eval (Stack [])
                [ Literal $ VInt 3
                , Literal $ VInt 1
                , Literal $ VInt 1
                , Quote [ add ]
                , vdip ])]

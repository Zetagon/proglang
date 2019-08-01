-- |

module Level1.BuiltIn where

import Level1.Types
import Level1.Eval

add :: Expr
add = BuiltinWord $ do
  x <- pop
  y <- pop
  case (x,y) of
    (Literal (VInt x'), Literal (VInt y')) -> push (Literal $ VInt $ x' + y')

sub = BuiltinWord $ do
  x <- pop
  y <- pop
  case (x,y) of
    (Literal (VInt x'), Literal (VInt y')) -> push (Literal $ VInt $ x' - y')

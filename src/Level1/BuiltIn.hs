-- |

module Level1.BuiltIn where

import Level1.Types

add :: Expr
add = BuiltinWord (\(Stack ((VInt x):(VInt y):xs)) -> Stack $ (VInt $ x + y):xs)

sub = BuiltinWord (\(Stack ((VInt x):(VInt y):xs)) -> Stack $ (VInt $ x - y):xs)

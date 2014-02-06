module Butterfly.AST (AST(..)) where
data AST = IntConstant Integer | If AST AST AST | While AST AST | Seq AST AST | Say AST
	deriving (Show)

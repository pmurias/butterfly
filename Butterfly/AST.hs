module Butterfly.AST (AST(..)) where
data AST = IntConstant Integer | If AST AST AST | While AST AST
	deriving (Show)

module Butterfly.AST (AST(..)) where
data AST = IntConstant Integer | If AST AST AST | While AST AST | Seq AST AST | Say AST | Gather AST | Take AST | Eager AST | At AST AST | Var [Char] | Assign AST AST | Decl [Char]
	deriving (Show)

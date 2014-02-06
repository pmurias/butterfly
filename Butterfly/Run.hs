module Butterfly.Run (eval, emptyHeap, runD, D(..)) where
import Butterfly.AST

data Heap = Heap

data D = Term Val | Output Val D

emptyHeap = Heap

runD (Term val) = print val
runD (Output val rest) = print val >> runD rest

data Val = Integer Integer | Nil
   deriving (Show,Eq)

type Cont = (Val -> Heap -> D)

isTrue :: Val -> Bool
isTrue (Integer i) = not (i == 0)

eval :: AST -> Heap -> Cont -> D

eval (IntConstant i) heap next = next (Integer i) heap
eval (If cond then' else') heap next = 
    eval cond heap (\condVal heap' -> if (isTrue condVal) then eval then' heap' next else eval else' heap' next)
eval (Seq a b) heap next = eval a heap (\_ heap' -> eval b heap next)
eval (Say arg) heap next = eval arg heap (\val heap' -> Output val (next Nil heap'))




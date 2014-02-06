module Butterfly.Run (eval, emptyHeap, runD, D(..)) where
import Butterfly.AST
import Data.Map as M

data Heap = Heap (M.Map Int PartialList)

insertPartial :: Heap -> Int -> PartialList -> Heap

insertPartial (Heap partials) cell partial = Heap $ M.insert cell partial partials

newPartial :: Heap -> Int
newPartial (Heap partials) = M.size partials

adjustPartial :: Heap -> Int -> (PartialList -> PartialList) -> Heap
adjustPartial (Heap partials) cell transform = Heap $ M.adjust transform cell partials

data D = Term Val | Output Val D

data Val = Integer Integer | Nil | LazyListRef Int
   deriving (Show,Eq)

type Cont = (Heap -> Val -> D)

data PartialList = Cons Val PartialList | EndOfList | RestOfList (Heap -> Int -> Cont -> D)

getElement :: Heap -> PartialList -> Int -> Cont -> D

getElement heap (Cons val _) 0 next = next heap val
getElement heap (Cons val rest) i next = getElement heap rest (i-1) next
getElement heap EndOfList _ next = next heap Nil
getElement heap (RestOfList listCont) i next = listCont heap i next

emptyHeap = Heap (M.empty)

runD (Term val) = print val
runD (Output val rest) = print val >> runD rest


isTrue :: Val -> Bool
isTrue (Integer i) = not (i == 0)

endOfList :: Int -> Cont -> Cont
endOfList cell next = \heap val -> next (adjustPartial heap cell filledList) Nil

filledList :: PartialList -> PartialList
filledList (Cons val rest) = Cons val (filledList rest)
filledList (RestOfList _) = EndOfList

eval :: AST -> Heap -> Cont -> D

eval (IntConstant i) heap next = next heap (Integer i) 
eval (If cond then' else') heap next = 
    eval cond heap (\heap' condVal  -> if (isTrue condVal) then eval then' heap' next else eval else' heap' next)
eval (Seq a b) heap next = eval a heap (\heap' _ -> eval b heap next)
eval (Say arg) heap next = eval arg heap (\heap' val -> Output val (next heap' Nil))

eval while@(While cond body) heap next = eval cond heap (\heap' val -> if isTrue val then (eval body heap' (\heap'' _ -> eval while heap'' next)) else next heap Nil)

eval (Gather body) heap next = 
	let newCell = newPartial heap
            heap' = insertPartial heap newCell (RestOfList (\heap i result -> eval body heap (endOfList newCell result))) in
	next heap' (LazyListRef newCell)




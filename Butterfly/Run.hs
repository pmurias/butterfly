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

getPartial :: Heap -> Int -> PartialList
getPartial (Heap partials) cell = M.findWithDefault (error "missing list cell") cell partials

data D = Term Heap Val | Output Heap Val D

data Val = Integer Integer | Nil | LazyListRef Int
    deriving (Show,Eq)

instance PrettyPrint Val where
    pretty heap (Integer i) = show i
    pretty heap (Nil) = "Nil"
    pretty heap (LazyListRef ref) = "["++(pretty heap (getPartial heap ref))++"]"

instance PrettyPrint PartialList where
	pretty heap EndOfList = ""
	pretty heap (RestOfList _) = "..."
	pretty heap (Cons val rest) = (pretty heap val)++","++(pretty heap rest)

class PrettyPrint a where 
     pretty :: Heap -> a -> String

type Cont = (Heap -> Val -> D)

data PartialList = Cons Val PartialList | EndOfList | RestOfList (Heap -> Int -> Cont -> D)

getElement :: Heap -> PartialList -> Int -> Cont -> D

getElement heap (Cons val _) 0 next = next heap val
getElement heap (Cons val rest) i next = getElement heap rest (i-1) next
getElement heap EndOfList _ next = next heap Nil
getElement heap (RestOfList listCont) i next = listCont heap i next

emptyHeap = Heap (M.empty)


runD (Term heap val) = return ()
runD (Output heap val rest) = putStrLn (pretty heap val) >> runD rest


isTrue :: Val -> Bool
isTrue (Integer i) = not (i == 0)

endOfList :: Int -> Cont -> Cont
endOfList cell next = \heap val -> next (adjustPartial heap cell filledList) Nil

filledList :: PartialList -> PartialList
filledList (Cons val rest) = Cons val (filledList rest)
filledList (RestOfList _) = EndOfList

eager :: Heap -> Val -> Cont -> D

eager heap val@(LazyListRef ref) next = eager' heap val 0 (getPartial heap ref) next
  
eager' :: Heap -> Val -> Int -> PartialList -> Cont -> D

eager' heap val i (Cons _ rest) next = eager' heap val (i+i) rest next
eager' heap val _ EndOfList next = next heap val
eager' heap val i (RestOfList listCont) next = listCont heap i (\heap' _ -> eager heap' val next)


{-
eager heap val@(LazyListRef ref) next =

	let list = findPartial heap ref in eager list

            heap' = eager' heap 0 list
            in next heap' val
	

---	RestOfList (Heap -> Int -> Cont -> D)
eager heap (Cons val rest) i next = getElement heap rest (i-1) next
getElement heap EndOfList _ next = next heap Nil
--getElement heap (RestOfList listCont) i next = listCont heap i next
-}



eval :: AST -> Heap -> Cont -> D

eval (IntConstant i) heap next = next heap (Integer i) 
eval (If cond then' else') heap next = 
    eval cond heap (\heap' condVal  -> if (isTrue condVal) then eval then' heap' next else eval else' heap' next)
eval (Seq a b) heap next = eval a heap (\heap' _ -> eval b heap next)
eval (Say arg) heap next = eval arg heap (\heap' val -> Output heap' val (next heap' Nil))

eval while@(While cond body) heap next = eval cond heap (\heap' val -> if isTrue val then (eval body heap' (\heap'' _ -> eval while heap'' next)) else next heap Nil)

eval (Gather body) heap next = 
	let newCell = newPartial heap
            heap' = insertPartial heap newCell (RestOfList (\heap i result -> eval body heap (endOfList newCell result))) in
	next heap' (LazyListRef newCell)

eval (Eager arg) heap next = eval arg heap (\heap' val -> eager heap' val next)



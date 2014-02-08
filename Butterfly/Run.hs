module Butterfly.Run (eval, emptyHeap, runD, D(..) ) where
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

data PartialList = Cons Val PartialList | EndOfList | RestOfList (Heap -> (Heap -> D) -> D)

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

eager heap val@(LazyListRef ref) next = eager' heap val (getPartial heap ref) next
  
eager' :: Heap -> Val -> PartialList -> Cont -> D

eager' heap val (Cons _ rest) next = eager' heap val rest next
eager' heap val EndOfList next = next heap val
eager' heap val (RestOfList computeRest) next = computeRest heap (\heap' -> eager heap' val next)

-- RestOfList (Heap -> (Heap -> D) -> D)

pushOnList :: Int -> (Heap -> D) -> Heap -> Val -> Cont -> D
pushOnList cell result heap val getMore = result (error "...")

type GatherCont = Heap -> Val -> Cont -> D

eval :: AST -> Heap -> GatherCont -> Cont -> D

eval (IntConstant i) heap gather next = next heap (Integer i) 
eval (If cond then' else') heap gather next = 
    eval cond heap gather (\heap' condVal  -> if (isTrue condVal) then eval then' heap' gather next else eval else' heap' gather next)
eval (Seq a b) heap gather next = eval a heap gather (\heap' _ -> eval b heap gather next)
eval (Say arg) heap gather next = eval arg heap gather (\heap' val -> Output heap' val (next heap' Nil))

eval while@(While cond body) heap gather next = eval cond heap gather (\heap' val -> if isTrue val then (eval body heap' gather (\heap'' _ -> eval while heap'' gather next)) else next heap Nil)

eval (Gather body) heap gather next = 
	let newCell = newPartial heap
            heap' = insertPartial heap newCell (RestOfList (\heap result -> eval body heap (pushOnList newCell result) (endOfList newCell (\heap val -> result heap)))) in next heap' (LazyListRef newCell)

eval (Eager arg) heap gather next = eval arg heap gather (\heap' val -> eager heap' val next)
--eval (Take arg) heap gather next = eval arg heap gather (\heap' val -> gather heap' val next)
eval (Take arg) heap gather next = gather heap (Integer 1) next



	module Butterfly.Run (eval, emptyHeap, runD, D(..), Val(..) ) where
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

	type Cont = (Heap -> (Heap -> D) -> Val -> D)

	data PartialList = Cons Val PartialList | EndOfList | RestOfList (Heap -> (Heap -> D) -> D)

	emptyHeap = Heap (M.empty)


	runD (Term heap val) = return ()
	runD (Output heap val rest) = putStrLn (pretty heap val) >> runD rest


	isTrue :: Val -> Bool
	isTrue (Integer i) = not (i == 0)

	endOfList :: Int -> Cont -> Cont
	endOfList cell next = \heap gatherCont val -> next (adjustPartial heap cell filledList) gatherCont Nil

	filledList :: PartialList -> PartialList
	filledList (Cons val rest) = Cons val (filledList rest)
	filledList (RestOfList _) = EndOfList

	eager :: Heap -> Val -> GatherCont -> Cont -> D

	eager heap val@(LazyListRef ref) gatherCont next = eager' heap val (getPartial heap ref) gatherCont next 
	  
	eager' :: Heap -> Val -> PartialList -> GatherCont -> Cont -> D

	eager' heap val (Cons _ rest) gatherCont next = eager' heap val rest gatherCont next
	eager' heap val EndOfList gatherCont next = next heap gatherCont val
	eager' heap val (RestOfList computeRest) gatherCont next = computeRest heap (\heap' -> eager heap' val gatherCont next)

	-- RestOfList (Heap -> (Heap -> D) -> D)

	--pushOnList :: Int -> (Heap -> D) -> Heap -> Val -> Cont -> D
	--pushOnList cell result heap val getMore = result (error "...")

	type GatherCont = (Heap -> D)

	type Cell = Int

	extendList :: Heap -> Cell -> Cont -> Val -> Heap 

	extendList heap cell produceMore newVal =  adjustPartial heap cell traverse
		where traverse (Cons val rest) = Cons val (traverse rest)
		      traverse (RestOfList _) = Cons newVal (RestOfList (\heap consume -> produceMore heap consume Nil)) 

	eval :: AST -> Heap -> GatherCont -> Cont -> D
	eval (IntConstant i) heap gatherCont next = next heap gatherCont (Integer i) 
	eval (Take arg) heap gatherCont next = gatherCont (extendList heap 0 next (Integer 1))
	eval (Gather body) heap gatherCont next = 
		let newCell = 0
		    heap' = insertPartial heap newCell (RestOfList (\heap result -> eval body heap result (endOfList newCell (\heap' gatherCont' val -> result heap')))) in next heap' gatherCont (LazyListRef newCell)

	eval (Say arg) heap gatherCont next = eval arg heap gatherCont (\heap' gatherCont val -> Output heap' val (next heap' gatherCont Nil))

	eval (Eager arg) heap gatherCont next = eval arg heap gatherCont (\heap' gatherCont val -> eager heap' val gatherCont next)

	eval (Seq a b) heap gatherCont next = eval a heap gatherCont (\heap' gatherCont' _ -> eval b heap' gatherCont' next)

	eval (If cond then' else') heap gatherCont next = 
	    eval cond heap gatherCont (\heap' gatherCont' condVal  -> if (isTrue condVal) then eval then' heap' gatherCont' next else eval else' heap' gatherCont' next)

	eval while@(While cond body) heap gatherCont next = eval cond heap gatherCont (\heap' gatherCont' val -> if isTrue val then (eval body heap' gatherCont' (\heap'' gatherCont'' _ -> eval while heap'' gatherCont'' next)) else next heap gatherCont' Nil)

	{-


	eval (Gather body) heap gather next = 
		let newCell = newPartial heap
		    heap' = insertPartial heap newCell (RestOfList (\heap result -> eval body heap (pushOnList newCell result) (endOfList newCell (\heap val -> result heap)))) in next heap' (LazyListRef newCell)


	--eval (Take arg) heap gather next = eval arg heap gather (\heap' val -> gather heap' val next)
	-}



module Butterfly.Run (eval, emptyHeap, emptyEnv, runD, D(..), Val(..) ) where
import Butterfly.AST
import Data.Map as M

data Heap = Heap (M.Map Int PartialList) (M.Map Int Val)
type Env = M.Map String Int

insertPartial :: Heap -> Int -> PartialList -> Heap

insertPartial (Heap partials vals) cell partial = Heap (M.insert cell partial partials) vals

newPartial :: Heap -> Int
newPartial (Heap partials vals) = M.size partials

adjustPartial :: Heap -> Int -> (PartialList -> PartialList) -> Heap
adjustPartial (Heap partials vals) cell transform = Heap (M.adjust transform cell partials) vals
getPartial :: Heap -> Int -> PartialList
getPartial (Heap partials vals) cell = M.findWithDefault (error "missing list cell") cell partials

getVarCell :: Env -> String -> Cell
getVarCell env name = M.findWithDefault (error "missing var cell mapping") name env


getVar :: Env -> Heap -> String -> Val
getVar env (Heap partials vals) name = M.findWithDefault (error "var cell pointing to nothing") (getVarCell env name) vals

setVar :: Env -> Heap -> String -> Val -> Heap
setVar env (Heap partials vals) name val = Heap partials (M.insert (getVarCell env name) val vals)

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

emptyHeap = Heap M.empty M.empty

emptyEnv :: Env
emptyEnv = M.empty


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

at :: Heap -> GatherCont -> Val -> Val -> Cont -> D
at heap gatherCont array@(LazyListRef cell) index@(Integer int) next = traverse int (getPartial heap cell) where	
	traverse 0 (Cons val rest) = next heap gatherCont val
	traverse i (Cons val rest) = traverse (i-1) rest
	traverse i EndOfList = next heap gatherCont Nil
	traverse i (RestOfList getMore) = getMore heap (\heap' -> at heap' gatherCont array index next)



type GatherCont = (Heap -> D)

type Cell = Int

extendList :: Heap -> Cell -> Cont -> Val -> Heap 

extendList heap cell produceMore newVal =  adjustPartial heap cell traverse
	where traverse (Cons val rest) = Cons val (traverse rest)
	      traverse (RestOfList _) = Cons newVal (RestOfList (\heap consume -> produceMore heap consume Nil)) 

declareVars :: Env -> Heap -> [String] -> (Env,Heap)
declareVars env (Heap partials vals) decls = let (env',vals') = declareVars' env vals decls in (env',Heap partials vals')

type VarHeap  = M.Map Int Val
declareVars' :: Env -> VarHeap -> [String] -> (Env,VarHeap)
declareVars' env heap [] = (env,heap)
declareVars' env heap (var:rest) = 
	let (env',heap') = declareVars' env heap rest
	    newCell = M.size heap'
	    in (M.insert var newCell env',M.insert newCell Nil heap')


eval :: AST -> Env -> Heap -> GatherCont -> Cont -> D

eval (IntConstant i) env heap gatherCont next = next heap gatherCont (Integer i) 
eval (Take arg) env heap gatherCont next = eval arg env heap gatherCont (\heap' gatherCont' val -> gatherCont' (extendList heap 0 next val))
eval (Gather body) env heap gatherCont next = 
	let newCell = 0
	    heap' = insertPartial heap newCell (RestOfList (\heap result -> eval body env heap result (endOfList newCell (\heap' gatherCont' val -> result heap')))) in next heap' gatherCont (LazyListRef newCell)

eval (Say arg) env heap gatherCont next = eval arg env heap gatherCont (\heap' gatherCont val -> Output heap' val (next heap' gatherCont Nil))

eval (Eager arg) env heap gatherCont next = eval arg env heap gatherCont (\heap' gatherCont val -> eager heap' val gatherCont next)

eval (Seq a b) env heap gatherCont next = eval a env heap gatherCont (\heap' gatherCont' _ -> eval b env heap' gatherCont' next)

eval (If cond then' else') env heap gatherCont next = 
    eval cond env heap gatherCont (\heap' gatherCont' condVal  -> if (isTrue condVal) then eval then' env heap' gatherCont' next else eval else' env heap' gatherCont' next)

eval while@(While cond body) env heap gatherCont next = eval cond env heap gatherCont (\heap' gatherCont' val -> if isTrue val then (eval body env heap' gatherCont' (\heap'' gatherCont'' _ -> eval while env heap'' gatherCont'' next)) else next heap gatherCont' Nil)

eval (At array index) env heap gatherCont next = eval array env heap gatherCont (\heap' gatherCont' array' -> eval index env heap' gatherCont' (\heap'' gatherCont'' index' -> at heap'' gatherCont'' array' index' next))

eval (Block decls stmts) env heap gatherCont next = let (env',heap') = declareVars env heap decls in
	eval stmts env' heap' gatherCont next
eval (Assign (Var name) rvalue) env heap gatherCont next = eval rvalue env heap gatherCont (\heap' gatherCont' val -> next (setVar env heap' name val) gatherCont' Nil)
eval (Var name) env heap gatherCont next = next heap gatherCont (getVar env heap name)


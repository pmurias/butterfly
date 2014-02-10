import Butterfly.Parser
import System.Environment
import Text.ParserCombinators.Parsec
import Butterfly.Run
main = do
          [input] <- getArgs
          parsed <- parseFromFile statementlist input
	  case parsed of
		Left err -> print err
		Right ast -> do
			print ast
		       	runD $ eval ast emptyEnv emptyHeap (\heap -> Term heap (Integer 666)) (\heap gatherCont val -> Term heap val)

	   

import Butterfly.Parser
import System.Environment
import Text.ParserCombinators.Parsec
import Butterfly.Run
main = do
          [input] <- getArgs
          parsed <- parseFromFile statementlist input
	  case parsed of
		Left err -> print err
		Right ast -> runD $ eval ast emptyHeap (\val heap -> Term val)
	   

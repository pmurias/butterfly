import Butterfly.Parser
import System.Environment
import Text.ParserCombinators.Parsec
main = do
          [input] <- getArgs
          parsed <- parseFromFile statementlist input
	  case parsed of
		Left err -> print err
		Right x -> print x
	   

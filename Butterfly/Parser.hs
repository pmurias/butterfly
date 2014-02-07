module Butterfly.Parser (statementlist) where
import Butterfly.AST
import Text.ParserCombinators.Parsec

symbol s = lexeme (string s)
lexeme p = do { x <- p; spaces; return x }

say = symbol "say" >> expr >>= return . Say
take = symbol "take" >> expr >>= return . Take
eager = symbol "eager" >> expr >>= return . Eager

gather = symbol "gather" >> blast >>= return . Gather

statement = statementControl <|> say <|> expr 

statementControl = ifStmt <|> whileStmt

statementlist :: CharParser st AST
statementlist = sepEndBy1 statement (symbol ";") >>= return . foldl1 Seq

blast = block <|> statement

xblock = block

block = do
          symbol "{"
          list <- statementlist
          symbol "}"
          return list

ifStmt :: CharParser st AST
ifStmt = do
	    symbol "if" 
	    cond <- expr
	    then' <- xblock
	    symbol "else" 
	    else' <- xblock
	    return $ If cond then' else'

-- TODO missing else, elsif

whileStmt :: CharParser st AST
whileStmt = do
             symbol "while"
             cond <- expr
             body <- xblock
             return $ While cond body

expr :: CharParser st AST
expr = Butterfly.Parser.take <|> gather <|> eager <|> number
number = integer
integer = decint


stmt = ifStmt <|> whileStmt
decint = lexeme $ do {ds <- many1 digit; return $ IntConstant (read ds)}




module Butterfly.Parser where
import Butterfly.AST
import Text.ParserCombinators.Parsec

symbol s = lexeme (string s)
lexeme p = do { x <- p; spaces; return x }

statement = statement <|> statementControl <|> expr

statementControl = ifStmt <|> whileStmt

statementlist :: CharParser st AST
statementlist = statement

xblock = block
block = do
          symbol "{"
          list <- statementlist
          symbol "}"
          return list

ifStmt = symbol "if" >> xblock
-- TODO else, elsif

whileStmt = symbol "while" >> xblock

expr = number
number = integer
integer = decint


stmt = ifStmt <|> whileStmt
decint = lexeme $ do {ds <- many1 digit; return $ IntConstant (read ds)}




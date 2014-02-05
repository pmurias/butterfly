module Butterfly.Parser where
lexeme p = do { x <- p; spaces; return x }
import Text.ParseCombinators.Parsec

statement = statement <|> statementControl <|> expr

statementControl = ifStmt <|> whileStmt

xblock = block

ifStmt = symbol "if" >> xblock
-- TODO else, elsif

whileStmt = symbol "while" >> xblock

number = integer

integer = decint


stmt = ifStmt <|> whileStmt | <stmt>
expr = lexeme $ do {ds <- many1 digit; return $ IntConstant (read ds)}




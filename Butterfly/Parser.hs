module Butterfly.Parser (statementlist) where
import Butterfly.AST
import Text.ParserCombinators.Parsec

symbol s = lexeme (string s)
lexeme p = do { x <- p; spaces; return x }

varname = lexeme $ do
    sigil <- string "$"
    id <- many1 alphaNum 
    return $ sigil++id

var = varname >>= return . Var

assign = do
    lvalue <- var
    symbol "="
    rvalue <- expr
    return $ Assign lvalue rvalue


say = symbol "say" >> expr >>= return . Say
take = symbol "take" >> expr >>= return . Take
eager = symbol "eager" >> expr >>= return . Eager

gather = symbol "gather" >> blast >>= return . Gather

statement = statementControl <|> say <|> expr <|> block

statementControl = ifStmt <|> whileStmt

statementlist :: CharParser st AST
statementlist = sepEndBy1 statement (symbol ";") >>= return . foldl1 Seq

blast = block <|> statement

xblock = block
 
decls :: CharParser st [String]
decls = do
          symbol "my"
	  varnames <- sepEndBy1 varname (symbol ",")
	  symbol ";"
	  return varnames
	  
block = do
          symbol "{"
	  varnames <- option [] decls
          list <- statementlist
          symbol "}"
          return $ Block varnames list

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

at :: AST -> CharParser st AST
at array = do
            symbol "["
            index <- expr
            symbol "]"
	    return $ At array index

parens p = do {symbol "("; x <- p; symbol ")";return x}

expr :: CharParser st AST
expr = do
	array <- term
	option array (at array)

term :: CharParser st AST
term = Butterfly.Parser.take <|> gather <|> eager <|> number <|> (parens expr) <|> (try assign) <|> var

number = integer
integer = decint


stmt = ifStmt <|> whileStmt
decint = lexeme $ do {ds <- many1 digit; return $ IntConstant (read ds)}




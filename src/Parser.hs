module Parser
    ( parser
    ) where

import Lexer (LocatedToken, Token(..))

parser :: [LocatedToken] -> Either String Program
parser loctokens = case parse_program tokens of
    Just p -> Right p
    Nothing -> Left "Could not parse."
    where 
        tokens = map fst loctokens

data Program
    = Program Function  
    deriving (Show)

data Function
    = Function String [Statement]
    deriving (Show)

data Statement
    = ReturnStatement Expression
    deriving (Show)

data Expression
    = Constant Int
    deriving (Show)

parse_program :: [Token] -> Maybe Program
parse_program tokens = do
    (fun, rest) <- parse_function tokens
    case rest of 
        [] -> return (Program fun)
        _ -> Nothing

parse_function :: [Token] -> Maybe (Function, [Token])
parse_function (TokKeyInt:TokIdent name:TokOpenParen:TokKeyVoid:TokCloseParen:TokOpenBrace:tail) = do
    (stmt, rest) <- parse_statement tail
    case rest of 
        TokCloseBrace:rest' -> return (Function name [stmt], rest')
        _ -> Nothing
parse_function _ = Nothing

parse_statement :: [Token] -> Maybe (Statement, [Token])
parse_statement (TokKeyReturn:tail) = do
    (expr, rest) <- parse_expression tail
    case rest of
        TokSemicolon:rest' -> return (ReturnStatement expr, rest')
        _ -> Nothing
parse_statement _ = Nothing

parse_expression :: [Token] -> Maybe (Expression, [Token])
parse_expression ((TokInt n):tail) = return (Constant n, tail)
parse_expression _ = Nothing


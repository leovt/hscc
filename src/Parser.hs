module Parser
    ( parser
    , Program(..)
    , Function(..)
    , Statement(..)
    , Expression(..)
    , UnaryOperator(..)
    , BinaryOperator(..)
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
    | Unary UnaryOperator Expression
    | Binary BinaryOperator Expression Expression
    deriving (Show)

data UnaryOperator
    = Complement
    | Negate
    | LogicNot
    deriving (Show)

data BinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Remainder
    | BitAnd
    | BitOr
    | BitXor
    | ShiftLeft
    | ShiftRight
    | LogicAnd
    | LogicOr
    | Equal
    | NotEqual
    | Less
    | Greater
    | LessOrEqual
    | GreaterOrEqual
    deriving (Show)

binop :: Token -> Maybe BinaryOperator
binop TokAsterisk   = Just Multiply
binop TokSlash      = Just Divide
binop TokPercent    = Just Remainder
binop TokPlus       = Just Add
binop TokMinus      = Just Subtract
binop TokAmpersand  = Just BitAnd
binop TokPipe       = Just BitOr
binop TokCircumflex = Just BitXor
binop TokDblLess    = Just ShiftLeft
binop TokDblGreater = Just ShiftRight
binop TokDblAmp     = Just LogicAnd
binop TokDblPipe    = Just LogicOr
binop TokDblEqual   = Just Equal
binop TokBangEqual  = Just NotEqual
binop TokLess       = Just Less
binop TokGreater    = Just Greater
binop TokLessEqual  = Just LessOrEqual
binop TokGreaterEqual = Just GreaterOrEqual
binop _             = Nothing

precedence :: BinaryOperator -> Int
precedence Multiply    = 50
precedence Divide      = 50
precedence Remainder   = 50
precedence Add         = 45
precedence Subtract    = 45
precedence ShiftLeft   = 40
precedence ShiftRight  = 40
precedence Greater     = 35
precedence Less        = 35
precedence GreaterOrEqual = 35
precedence LessOrEqual = 35
precedence Equal       = 30
precedence NotEqual    = 30
precedence BitAnd      = 25
precedence BitXor      = 24
precedence BitOr       = 23
precedence LogicAnd    = 20
precedence LogicOr     = 18

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

parse_factor :: [Token] -> Maybe (Expression, [Token])
parse_factor ((TokInt n):tail) = return (Constant n, tail)
parse_factor (TokMinus:tail) = do
    (expr, rest) <- parse_factor tail
    return (Unary Negate expr, rest)
parse_factor (TokTilde:tail) = do
    (expr, rest) <- parse_factor tail
    return (Unary Complement expr, rest)
parse_factor (TokBang:tail) = do
    (expr, rest) <- parse_factor tail
    return (Unary LogicNot expr, rest)
parse_factor (TokOpenParen:tail) = do
    (expr, rest) <- parse_expression tail
    case rest of
        TokCloseParen:rest' -> return (expr, rest')
        _ -> Nothing
parse_factor _ = Nothing

parse_expression :: [Token] -> Maybe (Expression, [Token])
parse_expression = parse_expression_prec 0 
    where
        parse_expression_prec :: Int -> [Token] -> Maybe (Expression, [Token])
        parse_expression_prec min_prec tokens = do
            (left, rest) <- parse_factor tokens
            parse_rhs min_prec left rest

        parse_rhs :: Int -> Expression -> [Token] -> Maybe (Expression, [Token])
        parse_rhs min_prec left (token:rest) = 
            case binop token of
                Just operator -> 
                    let prec = precedence operator
                    in if prec < min_prec 
                        then Just (left, (token:rest))
                        else do
                            (right, rest') <- parse_expression_prec (1 + prec) rest
                            parse_rhs min_prec (Binary operator left right) rest'
                Nothing -> Just (left, (token:rest))
        parse_rhs _ left [] = Just (left, [])

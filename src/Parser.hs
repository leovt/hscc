module Parser
    ( parser
    , Program(..)
    , Function(..)
    , BlockItem(..)
    , Statement(..)
    , Declaration(..)
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
    = Function String [BlockItem]
    deriving (Show)

data BlockItem
    = Stmt Statement
    | Decl Declaration
    deriving (Show)

data Declaration 
    = VariableDeclaration String (Maybe Expression)
    deriving (Show)

data Statement
    = ReturnStatement Expression
    | ExpressionStatement Expression
    | NullStatement
    deriving (Show)

data Expression
    = Constant Int
    | Variable String
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
    | Assignment
    | CompoundAssignment BinaryOperator
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
binop TokEqual      = Just Assignment
binop TokPlusEqual       = Just (CompoundAssignment Add)
binop TokMinusEqual      = Just (CompoundAssignment Subtract)
binop TokAsteriskEqual   = Just (CompoundAssignment Multiply)
binop TokSlashEqual      = Just (CompoundAssignment Divide)
binop TokPercentEqual    = Just (CompoundAssignment Remainder)
binop TokAmpersandEqual  = Just (CompoundAssignment BitAnd)
binop TokPipeEqual       = Just (CompoundAssignment BitOr)
binop TokCircumflexEqual = Just (CompoundAssignment BitXor)
binop TokDblLessEqual    = Just (CompoundAssignment ShiftLeft)
binop TokDblGreaterEqual = Just (CompoundAssignment ShiftRight)
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
precedence Assignment  = 1
precedence (CompoundAssignment _) = 1

associativity :: BinaryOperator -> Int
associativity op = case op of
    Assignment -> 0 -- right_associative
    CompoundAssignment _ -> 0 -- compound assignments are also right-associative
    _other -> 1 -- left_associative

parse_program :: [Token] -> Maybe Program
parse_program tokens = do
    (fun, rest) <- parse_function tokens
    case rest of 
        [] -> return (Program fun)
        _ -> Nothing

parse_function :: [Token] -> Maybe (Function, [Token])
parse_function (TokKeyInt:TokIdent name:TokOpenParen:TokKeyVoid:TokCloseParen:TokOpenBrace:tail) = do
    (items, rest) <- parse_blockitems tail
    case rest of 
        TokCloseBrace:rest' -> return (Function name items, rest')
        _ -> Nothing
parse_function _ = Nothing

parse_blockitems :: [Token] -> Maybe ([BlockItem], [Token])
parse_blockitems tokens = Just (parse_blockitems_seq ([], tokens))
    where
        parse_blockitems_seq :: ([BlockItem], [Token]) -> ([BlockItem], [Token])
        parse_blockitems_seq (items, TokCloseBrace:tokens) = (items, TokCloseBrace:tokens)
        parse_blockitems_seq (items, tokens) = 
            case parse_statement tokens of
                Just (stmt, rest) -> parse_blockitems_seq (items ++ [Stmt stmt], rest)
                Nothing -> case parse_declaration tokens of
                    Just (decl, rest) -> parse_blockitems_seq (items ++ [Decl decl], rest)
                    Nothing -> error $ "expected block item " ++ show tokens ++ "\n" ++ show (parse_statement tokens)

parse_statement :: [Token] -> Maybe (Statement, [Token])
parse_statement (TokKeyReturn:tail) = do
    (expr, rest) <- parse_expression tail
    case rest of
        TokSemicolon:rest' -> return (ReturnStatement expr, rest')
        _ -> Nothing
parse_statement (TokSemicolon:tail) = do 
    return (NullStatement, tail)
parse_statement tokens = do 
    (expr, rest) <- parse_expression tokens
    case rest of
        TokSemicolon:rest' -> return (ExpressionStatement expr, rest')
        _ -> Nothing

parse_declaration :: [Token] -> Maybe (Declaration, [Token])
parse_declaration (TokKeyInt:(TokIdent name):tokens) = case tokens of
    (TokEqual:rest) -> do
        (expr, rest') <- parse_expression rest
        case rest' of
            TokSemicolon:rest'' -> return (VariableDeclaration name (Just expr), rest'')
            _ -> Nothing
    (TokSemicolon:rest) -> Just (VariableDeclaration name Nothing, rest)
    _ -> Nothing
parse_declaration _ = Nothing
    

parse_factor :: [Token] -> Maybe (Expression, [Token])
parse_factor ((TokInt n):tail) = return (Constant n, tail)
parse_factor ((TokIdent n):tail) = return (Variable n, tail)
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
                        assoc = associativity operator
                    in if prec < min_prec 
                        then Just (left, token:rest)
                        else do
                            (right, rest') <- parse_expression_prec (assoc + prec) rest
                            parse_rhs min_prec (Binary operator left right) rest'
                Nothing -> Just (left, token:rest)
        parse_rhs _ left [] = Just (left, [])

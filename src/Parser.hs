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
    | IfStatement Expression Statement (Maybe Statement)
    | NullStatement
    deriving (Show)

data Expression
    = Constant Int
    | Variable String
    | Unary UnaryOperator Expression
    | Binary BinaryOperator Expression Expression
    | Conditional Expression Expression Expression
    deriving (Show)

data UnaryOperator
    = Complement
    | Negate
    | LogicNot
    | PreIncrement
    | PreDecrement
    | PostIncrement
    | PostDecrement
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
            case parseStatement tokens of
                Just (stmt, rest) -> parse_blockitems_seq (items ++ [Stmt stmt], rest)
                Nothing -> case parse_declaration tokens of
                    Just (decl, rest) -> parse_blockitems_seq (items ++ [Decl decl], rest)
                    Nothing -> error $ "expected block item " ++ show tokens ++ "\n" ++ show (parseStatement tokens)

parseStatement :: [Token] -> Maybe (Statement, [Token])
parseStatement (TokKeyReturn:tail) = do
    (expr, rest) <- parseExpression tail
    case rest of
        TokSemicolon:rest' -> return (ReturnStatement expr, rest')
        _ -> Nothing
parseStatement (TokSemicolon:tail) = do 
    return (NullStatement, tail)
parseStatement (TokKeyIf:tail) = do
    case tail of 
        TokOpenParen:rest -> do
            (condExpr, rest') <- parseExpression rest
            case rest' of 
                TokCloseParen:rest'' -> do
                    (thenStmt, rest''') <- parseStatement rest''
                    case rest''' of
                        TokKeyElse:rest'''' -> do
                            (elseStmt, rest''''') <- parseStatement rest''''
                            return (IfStatement condExpr thenStmt (Just elseStmt), rest''''')
                        _ -> return (IfStatement condExpr thenStmt Nothing, rest''')
                _ -> error "expected ')' after 'if (condition'"
        _ -> error "expected '(' after 'if'"
parseStatement tokens = do 
    (expr, rest) <- parseExpression tokens
    case rest of
        TokSemicolon:rest' -> return (ExpressionStatement expr, rest')
        _ -> Nothing

parse_declaration :: [Token] -> Maybe (Declaration, [Token])
parse_declaration (TokKeyInt:(TokIdent name):tokens) = case tokens of
    (TokEqual:rest) -> do
        (expr, rest') <- parseExpression rest
        case rest' of
            TokSemicolon:rest'' -> return (VariableDeclaration name (Just expr), rest'')
            _ -> Nothing
    (TokSemicolon:rest) -> Just (VariableDeclaration name Nothing, rest)
    _ -> Nothing
parse_declaration _ = Nothing
    
parseFactor :: [Token] -> Maybe (Expression, [Token])
parseFactor tokens = do
    (expr, rest) <- parseFactorPrefix tokens
    parseFactorSuffix expr rest
    where
        parseFactorSuffix :: Expression -> [Token] -> Maybe (Expression, [Token])
        parseFactorSuffix expr (TokDblPlus:rest) = parseFactorSuffix (Unary PostIncrement expr) rest
        parseFactorSuffix expr (TokDblMinus:rest) = parseFactorSuffix (Unary PostDecrement expr) rest
        parseFactorSuffix expr rest = Just (expr, rest)

parseFactorPrefix :: [Token] -> Maybe (Expression, [Token])
parseFactorPrefix ((TokInt n):tail) = return (Constant n, tail)
parseFactorPrefix ((TokIdent n):tail) = return (Variable n, tail)
parseFactorPrefix (TokMinus:tail) = do
    (expr, rest) <- parseFactor tail
    return (Unary Negate expr, rest)
parseFactorPrefix (TokTilde:tail) = do
    (expr, rest) <- parseFactor tail
    return (Unary Complement expr, rest)
parseFactorPrefix (TokBang:tail) = do
    (expr, rest) <- parseFactor tail
    return (Unary LogicNot expr, rest)
parseFactorPrefix (TokDblPlus:tail) = do
    (expr, rest) <- parseFactor tail
    return (Unary PreIncrement expr, rest)
parseFactorPrefix (TokDblMinus:tail) = do
    (expr, rest) <- parseFactor tail
    return (Unary PreDecrement expr, rest)
parseFactorPrefix (TokOpenParen:tail) = do
    (expr, rest) <- parseExpression tail
    case rest of
        TokCloseParen:rest' -> return (expr, rest')
        _ -> Nothing
parseFactorPrefix _ = Nothing

parseExpression :: [Token] -> Maybe (Expression, [Token])
parseExpression = parse_expression_prec 0 
    where
        parse_expression_prec :: Int -> [Token] -> Maybe (Expression, [Token])
        parse_expression_prec min_prec tokens = do
            (left, rest) <- parseFactor tokens
            parse_rhs min_prec left rest

        parse_rhs :: Int -> Expression -> [Token] -> Maybe (Expression, [Token])
        parse_rhs min_prec left (TokQuestion:rest) =
            let prec = 3
                assoc = 0
            in if prec < min_prec 
                then Just (left, TokQuestion:rest)
                else do
                    (midExpr, rest') <- parseExpression rest
                    case rest' of 
                        TokColon:rest'' -> do
                            (rightExpr, rest''') <- parse_expression_prec (assoc + prec) rest''
                            parse_rhs min_prec (Conditional left midExpr rightExpr) rest'''
                        _ -> error "expected ':' in conditional expression"
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


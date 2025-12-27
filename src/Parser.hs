module Parser
  ( parser,
    Program (..),
    Function (..),
    BlockItem (..),
    Statement (..),
    Declaration (..),
    Expression (..),
    UnaryOperator (..),
    BinaryOperator (..),
    Label (..),
  )
where

import Lexer (LocatedToken, Token (..))

parser :: [LocatedToken] -> Either String Program
parser loctokens = parseProgram (map fst loctokens)

{- HLINT ignore "Use newtype instead of data" -}
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

data Label
  = Label String
  deriving (Show)

data Statement
  = ReturnStatement Expression
  | ExpressionStatement Expression
  | IfStatement Expression Statement (Maybe Statement)
  | LabelledStatement Label Statement
  | GotoStatement String
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
binop TokAsterisk = Just Multiply
binop TokSlash = Just Divide
binop TokPercent = Just Remainder
binop TokPlus = Just Add
binop TokMinus = Just Subtract
binop TokAmpersand = Just BitAnd
binop TokPipe = Just BitOr
binop TokCircumflex = Just BitXor
binop TokDblLess = Just ShiftLeft
binop TokDblGreater = Just ShiftRight
binop TokDblAmp = Just LogicAnd
binop TokDblPipe = Just LogicOr
binop TokDblEqual = Just Equal
binop TokBangEqual = Just NotEqual
binop TokLess = Just Less
binop TokGreater = Just Greater
binop TokLessEqual = Just LessOrEqual
binop TokGreaterEqual = Just GreaterOrEqual
binop TokEqual = Just Assignment
binop TokPlusEqual = Just (CompoundAssignment Add)
binop TokMinusEqual = Just (CompoundAssignment Subtract)
binop TokAsteriskEqual = Just (CompoundAssignment Multiply)
binop TokSlashEqual = Just (CompoundAssignment Divide)
binop TokPercentEqual = Just (CompoundAssignment Remainder)
binop TokAmpersandEqual = Just (CompoundAssignment BitAnd)
binop TokPipeEqual = Just (CompoundAssignment BitOr)
binop TokCircumflexEqual = Just (CompoundAssignment BitXor)
binop TokDblLessEqual = Just (CompoundAssignment ShiftLeft)
binop TokDblGreaterEqual = Just (CompoundAssignment ShiftRight)
binop _ = Nothing

precedence :: BinaryOperator -> Int
precedence Multiply = 50
precedence Divide = 50
precedence Remainder = 50
precedence Add = 45
precedence Subtract = 45
precedence ShiftLeft = 40
precedence ShiftRight = 40
precedence Greater = 35
precedence Less = 35
precedence GreaterOrEqual = 35
precedence LessOrEqual = 35
precedence Equal = 30
precedence NotEqual = 30
precedence BitAnd = 25
precedence BitXor = 24
precedence BitOr = 23
precedence LogicAnd = 20
precedence LogicOr = 18
precedence Assignment = 1
precedence (CompoundAssignment _) = 1

associativity :: BinaryOperator -> Int
associativity op = case op of
  Assignment -> 0 -- right_associative
  CompoundAssignment _ -> 0 -- compound assignments are also right-associative
  _other -> 1 -- left_associative

parseProgram :: [Token] -> Either String Program
parseProgram tokens = do
  (fun, rest) <- parseFunction tokens
  case rest of
    [] -> return (Program fun)
    _ -> Left "unexpected tokens after function"

parseFunction :: [Token] -> Either String (Function, [Token])
parseFunction (TokKeyInt : TokIdent name : TokOpenParen : TokKeyVoid : TokCloseParen : TokOpenBrace : tail) = do
  (items, rest) <- parseBlockitems tail
  case rest of
    TokCloseBrace : rest' -> return (Function name items, rest')
    _ -> Left "expected '}' at end of function"
parseFunction _ = Left "expected function"

parseBlockitems :: [Token] -> Either String ([BlockItem], [Token])
parseBlockitems tokens = parse_blockitems_seq ([], tokens)
  where
    parse_blockitems_seq :: ([BlockItem], [Token]) -> Either String ([BlockItem], [Token])
    parse_blockitems_seq (items, TokCloseBrace : tokens) = Right (items, TokCloseBrace : tokens)
    parse_blockitems_seq (items, tokens) = do
      suite <- parseDeclaration tokens
      case suite of
        Just (decl, rest) -> parse_blockitems_seq (items ++ [Decl decl], rest)
        Nothing -> do
          suite' <- parseStatement tokens
          case suite' of
            Just (stmt, rest) -> parse_blockitems_seq (items ++ [Stmt stmt], rest)
            Nothing -> Left $ "expected block item " ++ show tokens ++ "\n" ++ show (parseStatement tokens)

parseStatement :: [Token] -> Either String (Maybe (Statement, [Token]))
parseStatement (TokKeyReturn : tail) = do
  (expr, rest) <- parseExpression tail
  case rest of
    TokSemicolon : rest' -> return (Just (ReturnStatement expr, rest'))
    _ -> Left "expected ';' after return statement"
parseStatement (TokSemicolon : tail) = do
  return (Just (NullStatement, tail))
parseStatement (TokKeyIf : tail) = do
  case tail of
    TokOpenParen : rest -> do
      (condExpr, rest') <- parseExpression rest
      case rest' of
        TokCloseParen : rest'' -> do
          suite <- parseStatement rest''
          case suite of
            Nothing -> Left "expected statement after if condition"
            Just (thenStmt, rest''') -> case rest''' of
              TokKeyElse : rest'''' -> do
                suite' <- parseStatement rest''''
                case suite' of
                  Nothing -> Left "expected statement after else"
                  Just (elseStmt, rest''''') -> return (Just (IfStatement condExpr thenStmt (Just elseStmt), rest'''''))
              _ -> return (Just (IfStatement condExpr thenStmt Nothing, rest'''))
        _ -> Left "expected ')' after 'if (condition'"
    _ -> Left "expected '(' after 'if'"
parseStatement (TokIdent labelName : TokColon : tail) = do
  let label = Label labelName
  suite <- parseStatement tail
  case suite of
    Nothing -> Left "expected statement after label"
    Just (stmt, rest) -> return (Just (LabelledStatement label stmt, rest))
parseStatement (TokKeyGoto : TokIdent labelName : tail) = do
  case tail of
    TokSemicolon : rest -> return (Just (GotoStatement labelName, rest))
    _ -> Left "expected ';' after 'goto label'"
parseStatement tokens = do
  (expr, rest) <- parseExpression tokens
  case rest of
    TokSemicolon : rest' -> return (Just (ExpressionStatement expr, rest'))
    _ -> Left "expected ';' after expression statement"

parseDeclaration :: [Token] -> Either String (Maybe (Declaration, [Token]))
parseDeclaration (TokKeyInt : (TokIdent name) : tokens) = case tokens of
  (TokEqual : rest) -> do
    (expr, rest') <- parseExpression rest
    case rest' of
      TokSemicolon : rest'' -> return (Just (VariableDeclaration name (Just expr), rest''))
      _ -> Left "expected ';' after variable declaration"
  (TokSemicolon : rest) -> return (Just (VariableDeclaration name Nothing, rest))
  _ -> Left "expected ';' or '=' after variable declaration"
parseDeclaration _ = Right Nothing

parseFactor :: [Token] -> Either String (Expression, [Token])
parseFactor tokens = do
  (expr, rest) <- parseFactorPrefix tokens
  parseFactorSuffix expr rest
  where
    parseFactorSuffix :: Expression -> [Token] -> Either String (Expression, [Token])
    parseFactorSuffix expr (TokDblPlus : rest) = parseFactorSuffix (Unary PostIncrement expr) rest
    parseFactorSuffix expr (TokDblMinus : rest) = parseFactorSuffix (Unary PostDecrement expr) rest
    parseFactorSuffix expr rest = Right (expr, rest)

parseFactorPrefix :: [Token] -> Either String (Expression, [Token])
parseFactorPrefix ((TokInt n) : tail) = return (Constant n, tail)
parseFactorPrefix ((TokIdent n) : tail) = return (Variable n, tail)
parseFactorPrefix (TokMinus : tail) = do
  (expr, rest) <- parseFactor tail
  return (Unary Negate expr, rest)
parseFactorPrefix (TokTilde : tail) = do
  (expr, rest) <- parseFactor tail
  return (Unary Complement expr, rest)
parseFactorPrefix (TokBang : tail) = do
  (expr, rest) <- parseFactor tail
  return (Unary LogicNot expr, rest)
parseFactorPrefix (TokDblPlus : tail) = do
  (expr, rest) <- parseFactor tail
  return (Unary PreIncrement expr, rest)
parseFactorPrefix (TokDblMinus : tail) = do
  (expr, rest) <- parseFactor tail
  return (Unary PreDecrement expr, rest)
parseFactorPrefix (TokOpenParen : tail) = do
  (expr, rest) <- parseExpression tail
  case rest of
    TokCloseParen : rest' -> return (expr, rest')
    _ -> Left "expected ')'"
parseFactorPrefix (token : _) = Left $ "unexpected token" ++ show token
parseFactorPrefix [] = Left "unexpected end of input"

parseExpression :: [Token] -> Either String (Expression, [Token])
parseExpression = parse_expression_prec 0
  where
    parse_expression_prec :: Int -> [Token] -> Either String (Expression, [Token])
    parse_expression_prec min_prec tokens = do
      (left, rest) <- parseFactor tokens
      parse_rhs min_prec left rest

    parse_rhs :: Int -> Expression -> [Token] -> Either String (Expression, [Token])
    parse_rhs min_prec left (TokQuestion : rest) =
      let prec = 3
          assoc = 0
       in if prec < min_prec
            then Right (left, TokQuestion : rest)
            else do
              (midExpr, rest') <- parseExpression rest
              case rest' of
                TokColon : rest'' -> do
                  (rightExpr, rest''') <- parse_expression_prec (assoc + prec) rest''
                  parse_rhs min_prec (Conditional left midExpr rightExpr) rest'''
                _ -> Left "expected ':' in conditional expression"
    parse_rhs min_prec left (token : rest) =
      case binop token of
        Just operator ->
          let prec = precedence operator
              assoc = associativity operator
           in if prec < min_prec
                then Right (left, token : rest)
                else do
                  (right, rest') <- parse_expression_prec (assoc + prec) rest
                  parse_rhs min_prec (Binary operator left right) rest'
        Nothing -> Right (left, token : rest)
    parse_rhs _ left [] = Right (left, [])

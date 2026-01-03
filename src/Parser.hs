module Parser
  ( parser,
    Program (..),
    FunctionDeclaration (..),
    VariableDeclaration (..),
    Block (..),
    BlockItem (..),
    Statement (..),
    Declaration (..),
    Expression (..),
    UnaryOperator (..),
    BinaryOperator (..),
    Label (..),
    ForInitializer (..),
  )
where

import Lexer (LocatedToken, Token (..))

parser :: [LocatedToken] -> Either String Program
parser loctokens = parseProgram (map fst loctokens)

{- HLINT ignore "Use newtype instead of data" -}
data Program
  = Program [Declaration]
  deriving (Show)

data BlockItem
  = Stmt Statement
  | Decl Declaration
  deriving (Show)

data ForInitializer
  = ForInitDecl Declaration
  | ForInitExpr Expression
  deriving (Show)

newtype Block = Block [BlockItem]
  deriving (Show)

data Declaration
  = VarDecl VariableDeclaration
  | FunDecl FunctionDeclaration
  deriving (Show)

data VariableDeclaration
  = VariableDeclaration String (Maybe Expression) StorageClass
  deriving (Show)

data FunctionDeclaration
  = FunctionDeclaration String [String] (Maybe Block) StorageClass
  deriving (Show)

data Label
  = Label String
  | CaseLabel Int
  | DefaultLabel
  deriving (Show)

data StorageClass
  = Static
  | Extern
  deriving (Show)

data Statement
  = ReturnStatement Expression
  | ExpressionStatement Expression
  | IfStatement Expression Statement (Maybe Statement)
  | LabelledStatement Label Statement
  | GotoStatement String
  | CompoundStatement Block
  | BreakStatement
  | ContinueStatement
  | WhileStatement Expression Statement
  | DoWhileStatement Expression Statement
  | ForStatement (Maybe ForInitializer) (Maybe Expression) (Maybe Expression) Statement
  | SwitchStatement Expression Statement
  | NullStatement
  deriving (Show)

data Expression
  = Constant Int
  | Variable String
  | Unary UnaryOperator Expression
  | Binary BinaryOperator Expression Expression
  | Conditional Expression Expression Expression
  | FunctionCall String [Expression]
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

isSpecifier :: Token -> Bool
isSpecifier TokKeyInt = True
isSpecifier TokKeyStatic = True
isSpecifier TokKeyExtern = True
isSpecifier _ = False

associativity :: BinaryOperator -> Int
associativity op = case op of
  Assignment -> 0 -- right_associative
  CompoundAssignment _ -> 0 -- compound assignments are also right-associative
  _other -> 1 -- left_associative

parseProgram :: [Token] -> Either String Program
parseProgram tokens = parseProgramSeq tokens []
  where
    parseProgramSeq :: [Token] -> [Declaration] -> Either String Program
    parseProgramSeq [] acc = Right (Program (reverse acc))
    parseProgramSeq tokens acc = do
      suite <- maybeParseDeclaration tokens
      case suite of
        Nothing -> Left "expected declaration"
        Just (fun, rest) -> parseProgramSeq rest (fun : acc)

parseFunction :: [Token] -> Either String (FunctionDeclaration, [Token])
parseFunction (TokKeyInt : TokIdent name : TokOpenParen : tail) = do
  let parse_params :: [String] -> [Token] -> Either String ([String], [Token])
      parse_params [] (TokKeyVoid : TokCloseParen : rest) = return ([], rest)
      parse_params [] (TokCloseParen : rest) = return ([], rest)
      parse_params params (TokKeyInt : TokIdent paramName : TokCloseParen : rest) = return (params ++ [paramName], rest)
      parse_params params (TokKeyInt : TokIdent paramName : TokComma : rest) = parse_params (params ++ [paramName]) rest
      parse_params _ _ = Left "expected parameter or ')'"
  (params, rest) <- parse_params [] tail
  case rest of
    TokOpenBrace : rest' -> do
      (block, rest'') <- parseBlock (TokOpenBrace : rest')
      return (FunctionDeclaration name params (Just block) Extern, rest'')
    TokSemicolon : rest -> return (FunctionDeclaration name params Nothing Extern, rest)
    _ -> Left "expected function body or ';' after function declaration"
parseFunction _ = Left "expected function declaration"

parseBlock :: [Token] -> Either String (Block, [Token])
parseBlock (TokOpenBrace : tokens) = do
  (items, rest) <- parseBlockitems tokens
  case rest of
    TokCloseBrace : rest' -> return (Block items, rest')
    _ -> Left "expected '}' at end of block"
parseBlock _ = Left "expected '{' at start of block"

parseBlockitems :: [Token] -> Either String ([BlockItem], [Token])
parseBlockitems tokens = parse_blockitems_seq ([], tokens)
  where
    parse_blockitems_seq :: ([BlockItem], [Token]) -> Either String ([BlockItem], [Token])
    parse_blockitems_seq (items, TokCloseBrace : tokens) = Right (items, TokCloseBrace : tokens)
    parse_blockitems_seq (items, tokens) = do
      suite <- maybeParseDeclaration tokens
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
parseStatement (TokKeyDefault : TokColon : tail) = do
  suite <- parseStatement tail
  case suite of
    Nothing -> Left "expected statement after default label"
    Just (stmt, rest) -> return (Just (LabelledStatement DefaultLabel stmt, rest))
parseStatement (TokKeyDefault : _) = Left "expected ':' after default"
parseStatement (TokKeyCase : TokInt n : TokColon : tail) = do
  let label = CaseLabel n
  suite <- parseStatement tail
  case suite of
    Nothing -> Left "expected statement after case label"
    Just (stmt, rest) -> return (Just (LabelledStatement label stmt, rest))
parseStatement (TokKeyCase : _) = Left "expected integer and ':' after case"
parseStatement (TokKeyGoto : TokIdent labelName : tail) = do
  case tail of
    TokSemicolon : rest -> return (Just (GotoStatement labelName, rest))
    _ -> Left "expected ';' after 'goto label'"
parseStatement (TokOpenBrace : tail) = do
  (block, rest) <- parseBlock (TokOpenBrace : tail)
  return (Just (CompoundStatement block, rest))
parseStatement (TokKeyBreak : tail) = do
  case tail of
    TokSemicolon : rest -> return (Just (BreakStatement, rest))
    _ -> Left "expected ';' after 'break'"
parseStatement (TokKeyContinue : tail) = do
  case tail of
    TokSemicolon : rest -> return (Just (ContinueStatement, rest))
    _ -> Left "expected ';' after 'continue'"
parseStatement (TokKeyWhile : TokOpenParen : tail) = do
  (expr, rest) <- parseExpression tail
  case rest of
    TokCloseParen : rest' -> do
      suite <- parseStatement rest'
      case suite of
        Nothing -> Left "expected statement after while condition"
        Just (stmt, rest'') -> return (Just (WhileStatement expr stmt, rest''))
    _ -> Left "expected ')' after while condition"
parseStatement (TokKeyWhile : _) = Left "expect ( after while"
parseStatement (TokKeyDo : tail) = do
  suite <- parseStatement tail
  case suite of
    Nothing -> Left "expected statement after do"
    Just (stmt, TokKeyWhile : TokOpenParen : rest) -> do
      (expr, rest') <- parseExpression rest
      case rest' of
        TokCloseParen : rest'' -> case rest'' of
          TokSemicolon : rest''' -> return (Just (DoWhileStatement expr stmt, rest'''))
          _ -> Left "expected ';' after do-while"
        _ -> Left "expected ')' after do-while condition"
    _ -> Left "expected 'while (' after do statement"
parseStatement (TokKeyFor : TokOpenParen : tail) = do
  -- parse initializer
  let parseInitializer :: [Token] -> Either String (Maybe ForInitializer, [Token])
      parseInitializer (TokSemicolon : tail) = return (Nothing, tail)
      parseInitializer tokens = do
        decl <- maybeParseDeclaration tokens
        case decl of
          Just (FunDecl _, _) -> Left "no function declaration in for initializer allowed."
          Just (d, rest) -> return (Just (ForInitDecl d), rest)
          Nothing -> do
            (expr, rest) <- parseExpression tokens
            case rest of
              TokSemicolon : rest' -> return (Just (ForInitExpr expr), rest')
              _ -> Left "expected ';' after for loop initializer"
  -- parse condition
  let parseCondition :: [Token] -> Either String (Maybe Expression, [Token])
      parseCondition (TokSemicolon : tail) = return (Nothing, tail)
      parseCondition tokens = do
        (expr, rest') <- parseExpression tokens
        case rest' of
          TokSemicolon : rest'' -> return (Just expr, rest'')
          _ -> Left "expected ';' after for loop condition"
  -- parse increment
  let parseIncrement :: [Token] -> Either String (Maybe Expression, [Token])
      parseIncrement (TokCloseParen : tail) = return (Nothing, tail)
      parseIncrement tokens = do
        (expr, rest') <- parseExpression tokens
        case rest' of
          TokCloseParen : rest'' -> return (Just expr, rest'')
          _ -> Left "expected ')' after for loop increment"
  (maybeInit, rest) <- parseInitializer tail
  (maybeCond, rest') <- parseCondition rest
  (maybeInc, rest'') <- parseIncrement rest'
  suite <- parseStatement rest''
  case suite of
    Nothing -> Left "expected statement after for loop"
    Just (stmt, rest''') -> return (Just (ForStatement maybeInit maybeCond maybeInc stmt, rest'''))
parseStatement (TokKeyFor : _) = Left "expect ( after for"
parseStatement (TokKeySwitch : TokOpenParen : tail) = do
  (expr, rest) <- parseExpression tail
  case rest of
    TokCloseParen : rest' -> do
      suite <- parseStatement rest'
      case suite of
        Nothing -> Left "expected statement after switch condition"
        Just (stmt, rest'') -> return (Just (SwitchStatement expr stmt, rest''))
    _ -> Left "expected ')' after switch condition"
parseStatement (TokKeySwitch : _) = Left "expect ( after switch"
parseStatement tokens = do
  (expr, rest) <- parseExpression tokens
  case rest of
    TokSemicolon : rest' -> return (Just (ExpressionStatement expr, rest'))
    _ -> Left "expected ';' after expression statement"

maybeParseDeclaration :: [Token] -> Either String (Maybe (Declaration, [Token]))
maybeParseDeclaration (tok : rest) =
  if isSpecifier tok
    then do
      decl <- parseDeclaration (tok : rest)
      return (Just decl)
    else return Nothing
maybeParseDeclaration _ = return Nothing

parseDeclaration :: [Token] -> Either String (Declaration, [Token])
parseDeclaration tokens@(TokKeyInt : TokIdent _ : TokOpenParen : _) = do
  (fun, rest) <- parseFunction tokens
  return (FunDecl fun, rest)
parseDeclaration tokens@(TokKeyInt : TokIdent _ : _) = do
  (decl, rest) <- parseVariableDeclaration tokens
  return (VarDecl decl, rest)
parseDeclaration _ = Left "expected declaration."

parseVariableDeclaration :: [Token] -> Either String (VariableDeclaration, [Token])
parseVariableDeclaration (TokKeyInt : (TokIdent name) : tokens) = case tokens of
  (TokEqual : rest) -> do
    (expr, rest') <- parseExpression rest
    case rest' of
      TokSemicolon : rest'' -> return (VariableDeclaration name (Just expr) Extern, rest'')
      _ -> Left "expected ';' after variable declaration"
  (TokSemicolon : rest) -> return (VariableDeclaration name Nothing Extern, rest)
  _ -> Left "expected ';' or '=' after variable declaration"
parseVariableDeclaration _ = Left "expected variable declaration"

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
parseFactorPrefix ((TokIdent n) : TokOpenParen : tail) = do
  let parse_arguments :: [Expression] -> [Token] -> Either String ([Expression], [Token])
      parse_arguments [] (TokCloseParen : rest) = return ([], rest)
      parse_arguments args tokens = do
        (expr, rest) <- parseExpression tokens
        case rest of
          TokComma : rest' -> parse_arguments (args ++ [expr]) rest'
          TokCloseParen : rest' -> return (args ++ [expr], rest')
          _ -> Left "expected ',' or ')' in function call arguments"
  (args, rest) <- parse_arguments [] tail
  return (FunctionCall n args, rest)
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

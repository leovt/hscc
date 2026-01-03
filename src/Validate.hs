module Validate
  ( validate,
    SwitchLabels (..),
    SymbolTable (..),
    SymbolInfo (..),
  )
where

import CTypes (CType (..))
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map
import Parser
  ( BinaryOperator (Assignment, CompoundAssignment),
    Block (..),
    BlockItem (..),
    Declaration (..),
    Expression (..),
    ForInitializer (..),
    Function (..),
    Label (..),
    Program (..),
    Statement (..),
    UnaryOperator (..),
  )

data LabelState
  = Defined String
  | Missing String
  | Resolved String
  deriving (Show, Eq)

data SwitchLabels
  = Case Int
  | Default
  deriving (Show, Eq, Ord)

data Linkage
  = InternalLinkage
  | ExternalLinkage
  | NoLinkage
  deriving (Show, Eq)

data ResolutionState = ResolutionState
  { nextID :: Int,
    names :: [Data.Map.Map String (String, Linkage)],
    labels :: Maybe (Data.Map.Map String LabelState),
    switchLabels :: [Data.Map.Map SwitchLabels ()],
    allowBreak :: Bool,
    allowContinue :: Bool
  }

{- HLINT ignore "Use newtype instead of data" -}
data TypecheckState = TypecheckState
  { symbolTable :: SymbolTable
  }

type ResM a = ExceptT String (State ResolutionState) a -- the resolution monad encapsulating the resolution state

type TypM a = ExceptT String (State TypecheckState) a -- the typechecking monad encapsulating the typechecking state

data SymbolState
  = SymDefined
  | SymDeclared
  deriving (Show, Eq)

data SymbolInfo
  = SymbolInfo
  { symbolType :: CType,
    symbolState :: SymbolState
  }
  deriving (Show)

newtype SymbolTable
  = SymbolTable (Data.Map.Map String SymbolInfo)
  deriving (Show)

validate :: Program -> Either String (Program, Int, SymbolTable)
validate program = do
  (resolved_program, nextID) <- resolve program
  (checked_program, symbolTable) <- typecheck resolved_program
  return (checked_program, nextID, symbolTable)

resolve :: Program -> Either String (Program, Int)
resolve program =
  case runState (runExceptT (resolveProgram program)) initState of
    (Left err, _) -> Left err
    (Right result, finalState) -> return (result, nextID finalState)
  where
    initState =
      ResolutionState
        { nextID = 1001,
          names = [Data.Map.empty],
          labels = Nothing,
          allowBreak = False,
          allowContinue = False,
          switchLabels = []
        }

    resolveNameDecl :: Linkage -> String -> ResM String
    resolveNameDecl NoLinkage name = do
      state <- get
      case names state of
        [] -> throwError $ "resolveNameDecl: not in a context (`" ++ name ++ "`)"
        (inner : rest) -> case Data.Map.lookup name inner of
          Just _ -> throwError $ "Duplicate declaration of " ++ name
          Nothing -> do
            let n = nextID state
                name' = name ++ "." ++ show n
                inner' = Data.Map.insert name (name', NoLinkage) inner
            put state {nextID = n + 1, names = inner' : rest}
            return name'
    resolveNameDecl linkage name = do
      state <- get
      case names state of
        [] -> throwError $ "resolveNameDecl: not in a context (`" ++ name ++ "`)"
        (inner : rest) -> case Data.Map.lookup name inner of
          Just (name', linkage') | linkage' == linkage -> return name'
          Just _ -> throwError $ "Duplicate declaration with conflicting linkage for " ++ name
          Nothing -> do
            let inner' = Data.Map.insert name (name, linkage) inner
            put state {names = inner' : rest}
            return name

    resolveName :: String -> ResM String
    resolveName name = do
      state <- get
      case lookupName (names state) of
        Just (name', _) -> return name'
        Nothing -> throwError $ "resolveName: not in a context for lookup of `" ++ name ++ "`"
      where
        lookupName [] = Nothing
        lookupName (scope : rest) = case Data.Map.lookup name scope of
          Just info -> Just info
          Nothing -> lookupName rest

    resolveLabelDecl :: String -> ResM String
    resolveLabelDecl name = do
      state <- get
      case labels state of
        Nothing -> throwError $ "not in a context for label declaration of `" ++ name ++ "`"
        (Just labels_map) -> case Data.Map.lookup name labels_map of
          Just (Resolved _) -> throwError $ "Duplicate declaration of label " ++ name
          Just (Defined _) -> throwError $ "Duplicate declaration of label " ++ name
          Just (Missing name') -> do
            put state {labels = Just (Data.Map.insert name (Resolved name') labels_map)}
            return name'
          Nothing -> do
            let n = nextID state
                name' = name ++ "." ++ show n
                labels_map' = Data.Map.insert name (Defined name') labels_map
            put state {nextID = n + 1, labels = Just labels_map'}
            return name'

    resolveLabel :: String -> ResM String
    resolveLabel name = do
      state <- get
      case labels state of
        Nothing -> throwError $ "not in a context for label lookup of `" ++ name ++ "`"
        (Just labels_map) -> case Data.Map.lookup name labels_map of
          Just (Resolved name') -> pure name'
          Just (Missing name') -> pure name'
          Just (Defined name') -> do
            put state {labels = Just (Data.Map.insert name (Resolved name') labels_map)}
            return name'
          Nothing -> do
            let n = nextID state
                name' = name ++ "." ++ show n
                labels_map' = Data.Map.insert name (Missing name') labels_map
            put state {nextID = n + 1, labels = Just labels_map'}
            return name'

    resolveProgram :: Program -> ResM Program
    resolveProgram (Program functions) = do
      functions' <- mapM resolveFunction functions
      return (Program functions')

    resolveFunction :: Function -> ResM Function
    resolveFunction (Function name params maybeBody) = do
      name' <- resolveNameDecl ExternalLinkage name
      state <- get
      let outer_names = names state
          {- if the list has at least two elements, we are already inside a function -}
          isNestedFunction (_ : _ : _) (Just _) = True
          isNestedFunction _ _ = False
      when (isNestedFunction outer_names maybeBody) $ throwError $ "Nested function definitions are not allowed." ++ show maybeBody
      put state {labels = Just Data.Map.empty, names = Data.Map.empty : outer_names} -- new scope for function locals
      params' <- mapM (resolveNameDecl NoLinkage) params
      maybeBody' <- traverse (resolveBlock False) maybeBody
      state <- get
      case labels state of
        Just labels_map -> do
          when (any isMissingLabel (Data.Map.elems labels_map)) $
            throwError $
              "Some labels were declared but not defined: " ++ show (filter isMissingLabel (Data.Map.elems labels_map))
        Nothing -> return ()
      put state {labels = Nothing, names = outer_names} -- pop function scope
      return (Function name' params' maybeBody')
    isMissingLabel :: LabelState -> Bool
    isMissingLabel (Missing _) = True
    isMissingLabel _ = False

    resolveBlock :: Bool -> Block -> ResM Block
    resolveBlock addScope (Block items) = do
      state <- get
      {- HLINT ignore "Use if" -}
      let outer_locals = names state
          inner_locals = case addScope of
            False -> outer_locals
            True -> Data.Map.empty : outer_locals
      put state {names = inner_locals} -- add an empty sub-scope
      items' <- mapM resolveBlockItem items
      state <- get
      put state {names = outer_locals} -- pop the sub-scope
      return (Block items')

    resolveBlockItem :: BlockItem -> ResM BlockItem
    resolveBlockItem (Decl (VariableDeclaration name init)) = do
      name' <- resolveNameDecl NoLinkage name
      init' <- mapM resolveExpression init
      return (Decl (VariableDeclaration name' init'))
    resolveBlockItem (Decl (FunctionDeclaration func)) = do
      func' <- resolveFunction func
      return (Decl (FunctionDeclaration func'))
    resolveBlockItem (Stmt stmt) = do
      stmt' <- resolveStatement stmt
      return (Stmt stmt')

    resolveStatement :: Statement -> ResM Statement
    resolveStatement (ReturnStatement expr) = do
      expr' <- resolveExpression expr
      return (ReturnStatement expr')
    resolveStatement (ExpressionStatement expr) = do
      expr' <- resolveExpression expr
      return (ExpressionStatement expr')
    resolveStatement (IfStatement cond thenStmt maybeElseStmt) = do
      cond' <- resolveExpression cond
      thenStmt' <- resolveStatement thenStmt
      maybeElseStmt' <- mapM resolveStatement maybeElseStmt
      return (IfStatement cond' thenStmt' maybeElseStmt')
    resolveStatement (LabelledStatement (Label label) stmt) = do
      stmt' <- resolveStatement stmt
      label' <- resolveLabelDecl label
      return (LabelledStatement (Label label') stmt')
    resolveStatement (LabelledStatement (CaseLabel n) stmt) = do
      checkSwitchLabels (Case n)
      stmt' <- resolveStatement stmt
      return (LabelledStatement (CaseLabel n) stmt')
    resolveStatement (LabelledStatement DefaultLabel stmt) = do
      checkSwitchLabels Default
      stmt' <- resolveStatement stmt
      return (LabelledStatement DefaultLabel stmt')
    resolveStatement (GotoStatement label) = do
      label' <- resolveLabel label
      return (GotoStatement label')
    resolveStatement (CompoundStatement block) = do
      block' <- resolveBlock True block
      return (CompoundStatement block')
    resolveStatement NullStatement = return NullStatement
    resolveStatement (WhileStatement cond stmt) = do
      cond' <- resolveExpression cond
      stmt' <- withLoopContext (resolveStatement stmt)
      return (WhileStatement cond' stmt')
    resolveStatement (DoWhileStatement cond stmt) = do
      cond' <- resolveExpression cond
      stmt' <- withLoopContext (resolveStatement stmt)
      return (DoWhileStatement cond' stmt')
    resolveStatement (ForStatement maybeInit maybeCond maybeInc stmt) = do
      state <- get
      let outer_locals = names state
      put state {names = Data.Map.empty : outer_locals} -- add an empty sub-scope
      maybeInit' <- case maybeInit of
        Nothing -> return Nothing
        Just (ForInitExpr expr) -> do
          expr' <- resolveExpression expr
          return (Just (ForInitExpr expr'))
        Just (ForInitDecl (VariableDeclaration name init)) -> do
          name' <- resolveNameDecl NoLinkage name
          init' <- mapM resolveExpression init
          return (Just (ForInitDecl (VariableDeclaration name' init')))
        Just (ForInitDecl _) -> throwError "Illegal for-loop initializer."
      maybeCond' <- mapM resolveExpression maybeCond
      maybeInc' <- mapM resolveExpression maybeInc
      stmt' <- withLoopContext (resolveStatement stmt)
      state <- get
      put state {names = outer_locals} -- pop the sub-scope
      return (ForStatement maybeInit' maybeCond' maybeInc' stmt')
    resolveStatement BreakStatement = do
      state <- get
      unless (allowBreak state) $ throwError "break outside of loop"
      return BreakStatement
    resolveStatement ContinueStatement = do
      state <- get
      unless (allowContinue state) $ throwError "continue outside of loop"
      return ContinueStatement
    resolveStatement (SwitchStatement expr stmt) = do
      {-      let isCompoundStatement (CompoundStatement _) = True
                isCompoundStatement _ = False
            unless (isCompoundStatement stmt) $
              throwError "switch statement body must be a compound statement" -}
      expr' <- resolveExpression expr
      state_before <- get
      put state_before {switchLabels = Data.Map.empty : switchLabels state_before, allowBreak = True}
      stmt' <- resolveStatement stmt
      state_after <- get
      put state_after {switchLabels = switchLabels state_before, allowBreak = allowBreak state_before}
      return (SwitchStatement expr' stmt')

    checkSwitchLabels :: SwitchLabels -> ResM ()
    checkSwitchLabels label = do
      state <- get
      case switchLabels state of
        [] -> throwError $ "not in a switch context: " ++ show label
        (current : rest) -> case Data.Map.lookup label current of
          Just _ -> throwError $ "duplicate label " ++ show label ++ " in switch context"
          Nothing -> do
            let current' = Data.Map.insert label () current
            put state {switchLabels = current' : rest}
            return ()

    withLoopContext :: ResM a -> ResM a
    withLoopContext action = do
      state_before <- get
      put state_before {allowBreak = True, allowContinue = True}
      result <- action
      state_after <- get
      put state_after {allowBreak = allowBreak state_before, allowContinue = allowContinue state_before}
      return result

    resolveExpression :: Expression -> ResM Expression
    resolveExpression (Variable name) = do
      name' <- resolveName name
      return (Variable name')
    resolveExpression (Unary PreDecrement (Variable var)) = do
      expr' <- resolveExpression (Variable var)
      return (Unary PreDecrement expr')
    resolveExpression (Unary PreIncrement (Variable var)) = do
      expr' <- resolveExpression (Variable var)
      return (Unary PreIncrement expr')
    resolveExpression (Unary PostDecrement (Variable var)) = do
      expr' <- resolveExpression (Variable var)
      return (Unary PostDecrement expr')
    resolveExpression (Unary PostIncrement (Variable var)) = do
      expr' <- resolveExpression (Variable var)
      return (Unary PostIncrement expr')
    resolveExpression (Unary PreDecrement _) = throwError "PreDecrement applied to non-variable."
    resolveExpression (Unary PreIncrement _) = throwError "PreIncrement applied to non-variable."
    resolveExpression (Unary PostDecrement _) = throwError "PostDecrement applied to non-variable."
    resolveExpression (Unary PostIncrement _) = throwError "PostIncrement applied to non-variable."
    resolveExpression (Unary op expr) = do
      expr' <- resolveExpression expr
      return (Unary op expr')
    resolveExpression (Binary Assignment (Variable left) right) = do
      left' <- resolveName left
      right' <- resolveExpression right
      return (Binary Assignment (Variable left') right')
    resolveExpression (Binary (CompoundAssignment op) (Variable left) right) = do
      left' <- resolveName left
      right' <- resolveExpression right
      return (Binary (CompoundAssignment op) (Variable left') right')
    resolveExpression (Binary Assignment _ _) = throwError "assign to non-variable."
    resolveExpression (Binary (CompoundAssignment _) _ _) = throwError "assign to non-variable."
    resolveExpression (Binary op left right) = do
      left' <- resolveExpression left
      right' <- resolveExpression right
      return (Binary op left' right')
    resolveExpression (Constant c) = pure (Constant c)
    resolveExpression (Conditional cond trueExpr falseExpr) = do
      cond' <- resolveExpression cond
      trueExpr' <- resolveExpression trueExpr
      falseExpr' <- resolveExpression falseExpr
      return (Conditional cond' trueExpr' falseExpr')
    resolveExpression (FunctionCall name args) = do
      name' <- resolveName name
      args' <- mapM resolveExpression args
      return (FunctionCall name' args')

typecheck :: Program -> Either String (Program, SymbolTable)
typecheck program = do
  case runState (runExceptT (tcProgram program)) initState of
    (Left err, _) -> Left err
    (Right result, finalState) -> return (result, symbolTable finalState)
  where
    initState =
      TypecheckState
        { symbolTable = SymbolTable Data.Map.empty
        }

    tcProgram :: Program -> TypM Program
    tcProgram (Program functions) = do
      functions' <- mapM tcFunction functions
      return (Program functions')

    tcFunction :: Function -> TypM Function
    tcFunction (Function name params maybeBody) = do
      let funcT = FuncT IntT (replicate (length params) IntT)
          thisState = case maybeBody of
            Just _ -> SymDefined
            Nothing -> SymDeclared

      state <- get
      let (SymbolTable symtab) = symbolTable state
      sinfo' <- case Data.Map.lookup name symtab of
        Just sinfo -> do
          when (funcT /= symbolType sinfo) $
            throwError $
              "Function " ++ name ++ " declared with different type."
          when (symbolState sinfo == SymDefined && thisState == SymDefined) $
            throwError $
              "Function " ++ name ++ " already defined." {- TODO: this belongs into the resolution phase -}
          let oldState = symbolState sinfo
              newState = case (oldState, thisState) of
                (SymDeclared, SymDeclared) -> SymDeclared
                _ -> SymDefined
          return $ SymbolInfo funcT newState
        Nothing -> return $ SymbolInfo funcT thisState

      let symtab' = Data.Map.insert name sinfo' symtab
      put state {symbolTable = SymbolTable symtab'}
      let tcParam :: String -> TypM String
          tcParam paramName = do
            state <- get
            let (SymbolTable symtab) = symbolTable state
                symbol = SymbolInfo IntT SymDefined
                symtab' = Data.Map.insert paramName symbol symtab
            put state {symbolTable = SymbolTable symtab'}
            return paramName
      params' <- mapM tcParam params
      maybeBody' <- traverse tcBlock maybeBody
      return (Function name params' maybeBody')

    tcDeclaration :: Declaration -> TypM Declaration
    tcDeclaration (VariableDeclaration name init) = do
      state <- get
      let (SymbolTable symtab) = symbolTable state
          varT = IntT {- TODO: variable types -}
          symbol = SymbolInfo varT SymDefined
          symtab' = Data.Map.insert name symbol symtab
      put state {symbolTable = SymbolTable symtab'}
      mapM_ (tcExpressionOf varT) init
      return (VariableDeclaration name init)
    tcDeclaration (FunctionDeclaration func) = do
      func' <- tcFunction func
      return (FunctionDeclaration func')

    tcBlock :: Block -> TypM Block
    tcBlock (Block items) = do
      items' <- mapM tcBlockItem items
      return (Block items')

    tcBlockItem :: BlockItem -> TypM BlockItem
    tcBlockItem (Decl decl) = do
      decl' <- tcDeclaration decl
      return (Decl decl')
    tcBlockItem (Stmt stmt) = do
      stmt' <- tcStatement stmt
      return (Stmt stmt')

    tcStatement :: Statement -> TypM Statement
    tcStatement (ReturnStatement expr) = do
      let retT = IntT {- TODO: function return type lookup -}
      tcExpressionOf retT expr
      return (ReturnStatement expr)
    tcStatement (ExpressionStatement expr) = do
      _ <- tcExpression expr
      return (ExpressionStatement expr)
    tcStatement (IfStatement cond thenStmt maybeElseStmt) = do
      tcExpressionOf IntT cond
      thenStmt' <- tcStatement thenStmt
      maybeElseStmt' <- mapM tcStatement maybeElseStmt
      return (IfStatement cond thenStmt' maybeElseStmt')
    tcStatement (LabelledStatement label stmt) = do
      stmt' <- tcStatement stmt
      return (LabelledStatement label stmt')
    tcStatement (GotoStatement label) = return (GotoStatement label)
    tcStatement (CompoundStatement block) = do
      block' <- tcBlock block
      return (CompoundStatement block')
    tcStatement NullStatement = return NullStatement
    tcStatement (WhileStatement cond stmt) = do
      tcExpressionOf IntT cond
      stmt' <- tcStatement stmt
      return (WhileStatement cond stmt')
    tcStatement (DoWhileStatement cond stmt) = do
      tcExpressionOf IntT cond
      stmt' <- tcStatement stmt
      return (DoWhileStatement cond stmt')
    tcStatement (ForStatement maybeInit maybeCond maybeInc stmt) = do
      maybeInit' <- case maybeInit of
        Nothing -> return Nothing
        Just (ForInitExpr expr) -> do
          _ <- tcExpression expr
          return (Just (ForInitExpr expr))
        Just (ForInitDecl decl) -> do
          decl' <- tcDeclaration decl
          return (Just (ForInitDecl decl'))
      mapM_ (tcExpressionOf IntT) maybeCond
      mapM_ tcExpression maybeInc
      stmt' <- tcStatement stmt
      return (ForStatement maybeInit' maybeCond maybeInc stmt')
    tcStatement BreakStatement = return BreakStatement
    tcStatement ContinueStatement = return ContinueStatement
    tcStatement (SwitchStatement expr stmt) = do
      tcExpressionOf IntT expr
      stmt' <- tcStatement stmt
      return (SwitchStatement expr stmt')

    tcExpression :: Expression -> TypM CType
    tcExpression (Variable name) = do
      state <- get
      let (SymbolTable symtab) = symbolTable state
      case Data.Map.lookup name symtab of
        Just sinfo -> return (symbolType sinfo)
        Nothing -> throwError $ "tcExpression: Undeclared variable " ++ name
    tcExpression (Unary _ expr) = do
      tcExpressionOf IntT expr
      return IntT
    tcExpression (Binary _ left right) = do
      leftT <- tcExpression left
      rightT <- tcExpression right
      unless (leftT == rightT) $
        throwError $
          "Type mismatch in binary operation: " ++ show leftT ++ " vs " ++ show rightT
      return leftT
    tcExpression (Constant _) = return IntT {- TODO: constants can have different types later -}
    tcExpression (Conditional cond trueExpr falseExpr) = do
      cond' <- tcExpression cond
      unless (cond' == IntT) $
        throwError $
          "Condition expression must be of type Int, got " ++ show cond'
      trueT <- tcExpression trueExpr
      falseT <- tcExpression falseExpr
      unless (trueT == falseT) $
        throwError $
          "Type mismatch in conditional expression: " ++ show trueT ++ " vs " ++ show falseT
      return trueT
    tcExpression (FunctionCall name args) = do
      argsT <- mapM tcExpression args
      funcT <- do
        state <- get
        let (SymbolTable symtab) = symbolTable state
        case Data.Map.lookup name symtab of
          Just sinfo -> return (symbolType sinfo)
          Nothing -> throwError $ "tcExpression: Undeclared function " ++ name
      case funcT of
        FuncT retT paramTs -> do
          unless (argsT == paramTs) $
            throwError $
              "Function "
                ++ name
                ++ " called with incorrect argument types: expected "
                ++ show paramTs
                ++ ", got "
                ++ show argsT
          return retT
        _ -> throwError $ "Type error: " ++ name ++ " is not a function."

    tcExpressionOf :: CType -> Expression -> TypM ()
    tcExpressionOf expectedType expr = do
      actualType <- tcExpression expr
      unless (actualType == expectedType) $
        throwError $
          "Type mismatch: expected "
            ++ show expectedType
            ++ ", got "
            ++ show actualType

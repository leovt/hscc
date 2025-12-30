module Validate
  ( validate,
    SwitchLabels (..),
  )
where

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

data TransState = TransState
  { nextID :: Int,
    names :: [Data.Map.Map String (String, Linkage)],
    labels :: Maybe (Data.Map.Map String LabelState),
    switchLabels :: [Data.Map.Map SwitchLabels ()],
    allowBreak :: Bool,
    allowContinue :: Bool
  }

type TransM a = ExceptT String (State TransState) a -- the translation monad encapsulating the translation state

newtype SymbolTable = SymbolTable ()

validate :: Program -> Either String (Program, Int, SymbolTable)
validate program = do
  (resolved_program, nextID) <- resolve program
  (checked_program, symbolTable) <- typecheck resolved_program
  return (checked_program, nextID, symbolTable)

typecheck :: Program -> Either String (Program, SymbolTable)
typecheck program = do
  -- currently a no-op
  return (program, SymbolTable ())

resolve :: Program -> Either String (Program, Int)
resolve program =
  case runState (runExceptT (resolveProgram program)) initState of
    (Left err, _) -> Left err
    (Right result, finalState) -> Right (result, nextID finalState)
  where
    initState =
      TransState
        { nextID = 1001,
          names = [Data.Map.empty],
          labels = Nothing,
          allowBreak = False,
          allowContinue = False,
          switchLabels = []
        }

    resolveNameDecl :: Linkage -> String -> TransM String
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

    resolveName :: String -> TransM String
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

    resolveLabelDecl :: String -> TransM String
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

    resolveLabel :: String -> TransM String
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

    resolveProgram :: Program -> TransM Program
    resolveProgram (Program functions) = do
      functions' <- mapM resolveFunction functions
      return (Program functions')

    resolveFunction :: Function -> TransM Function
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

    resolveBlock :: Bool -> Block -> TransM Block
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

    resolveBlockItem :: BlockItem -> TransM BlockItem
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

    resolveStatement :: Statement -> TransM Statement
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

    checkSwitchLabels :: SwitchLabels -> TransM ()
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

    withLoopContext :: TransM a -> TransM a
    withLoopContext action = do
      state_before <- get
      put state_before {allowBreak = True, allowContinue = True}
      result <- action
      state_after <- get
      put state_after {allowBreak = allowBreak state_before, allowContinue = allowContinue state_before}
      return result

    resolveExpression :: Expression -> TransM Expression
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

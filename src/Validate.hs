module Validate
  ( validate,
  )
where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map
import Data.Maybe (fromJust)
import Parser
  ( BinaryOperator (Assignment, CompoundAssignment),
    Block (..),
    BlockItem (..),
    Declaration (..),
    Expression (..),
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

data TransState = TransState
  { nextID :: Int,
    locals :: [Data.Map.Map String String],
    labels :: Maybe (Data.Map.Map String LabelState)
  }

type TransM a = ExceptT String (State TransState) a -- the translation monad encapsulating the translation state

resolveNameDecl :: String -> TransM String
resolveNameDecl name = do
  state <- get
  case locals state of
    [] -> throwError "not in a context"
    (inner : rest) -> case Data.Map.lookup name inner of
      Just _ -> throwError $ "Duplicate declaration of " ++ name
      Nothing -> do
        let n = nextID state
            name' = name ++ "." ++ show n
            inner' = Data.Map.insert name name' inner
        put state {nextID = n + 1, locals = inner' : rest}
        return name'

resolveName :: String -> TransM String
resolveName name = do
  state <- get
  case lookupName (locals state) of
    Just name' -> return name'
    Nothing -> throwError $ "not in a context for lookup of `" ++ name ++ "`"
  where
    lookupName [] = Nothing
    lookupName (scope : rest) = case Data.Map.lookup name scope of
      Just name' -> Just name'
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

validate :: Program -> Either String (Program, Int)
validate program =
  case runState (runExceptT (resolveProgram program)) initState of
    (Left err, _) -> Left err
    (Right result, finalState) -> Right (result, nextID finalState)
  where
    initState = TransState {nextID = 1001, locals = [], labels = Nothing}

    resolveProgram :: Program -> TransM Program
    resolveProgram (Program fun) = do
      fun' <- resolveFunction fun
      return (Program fun')

    resolveFunction :: Function -> TransM Function
    resolveFunction (Function name body) = do
      state <- get
      put state {labels = Just Data.Map.empty}
      body' <- resolveBlock body
      state <- get
      let labels_map = fromJust (labels state)
      when (any isMissingLabel (Data.Map.elems labels_map)) $
        throwError $
          "Some labels were declared but not defined: " ++ show (filter isMissingLabel (Data.Map.elems labels_map))
      put state {labels = Nothing}
      return (Function name body')

    isMissingLabel :: LabelState -> Bool
    isMissingLabel (Missing _) = True
    isMissingLabel _ = False

    resolveBlock :: Block -> TransM Block
    resolveBlock (Block items) = do
      state <- get
      let outer_locals = locals state
      put state {locals = Data.Map.empty : outer_locals} -- add an empty sub-scope
      items' <- mapM resolveBlockItem items
      state <- get
      put state {locals = outer_locals} -- pop the sub-scope
      return (Block items')

    resolveBlockItem :: BlockItem -> TransM BlockItem
    resolveBlockItem (Decl (VariableDeclaration name init)) = do
      name' <- resolveNameDecl name
      init' <- mapM resolveExpression init
      return (Decl (VariableDeclaration name' init'))
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
    resolveStatement (GotoStatement label) = do
      label' <- resolveLabel label
      return (GotoStatement label')
    resolveStatement NullStatement = return NullStatement

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

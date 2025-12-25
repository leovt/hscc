module Validate
    ( validate
    ) where

import Control.Monad.State
import qualified Data.Map

import Parser
    ( Program(..)
    , Function(..)
    , BlockItem(..)
    , Statement(..)
    , Expression(..)
    , Declaration(..)
    , BinaryOperator(Assignment)
    )

data TransState = TransState { nextID :: Int, locals :: [Data.Map.Map String String] }

type TransM a = State TransState a -- the translation monad encapsulating the translation state

resolveNameDecl :: String -> TransM String
resolveNameDecl name = do
    state <- get
    case locals state of
        [] -> error "not in a context"
        (inner:rest) -> case Data.Map.lookup name inner of
            Just _ -> error $ "Duplicate declaration of " ++ name
            Nothing -> do
                let n      = nextID state
                    name'  = name ++ "." ++ show n
                    inner' = Data.Map.insert name name' inner
                put state {nextID = n+1, locals = inner':rest}
                return name'

resolveName :: String -> TransM String
resolveName name = do
    state <- get
    return $ res name (locals state)
    where 
        res _ [] = error $ name ++ " not declared"
        res name (scope:rest) = case Data.Map.lookup name scope of
            Just name' -> name'
            Nothing -> res name rest


validate :: Program -> Either String (Program, Int)
validate program = 
    let (result, finalState) = runState (resolveProgram program) initState 
    in Right(result, nextID finalState)
        where
            initState = TransState { nextID = 1001, locals = [] }

            resolveProgram :: Program -> TransM Program
            resolveProgram (Program fun) = do 
                fun' <- resolveFunction fun
                return (Program fun')

            resolveFunction :: Function -> TransM Function
            resolveFunction (Function name items) = do
                state <- get
                let outer_locals = locals state
                put state {locals = Data.Map.empty:outer_locals} -- add an empty sub-scope
                items' <- mapM resolveBlockItem items
                put state {locals = outer_locals} -- add an empty sub-scope
                return (Function name items')

            resolveBlockItem :: BlockItem -> TransM BlockItem
            resolveBlockItem (Decl (VariableDeclaration name init)) = do
                name' <- resolveNameDecl name
                init' <- mapM resolveExpression init
                return (Decl (VariableDeclaration name' init'))
            resolveBlockItem (Stmt (ReturnStatement expr)) = do
                expr' <- resolveExpression expr
                return (Stmt (ReturnStatement expr'))
            resolveBlockItem (Stmt (ExpressionStatement expr)) = do
                expr' <- resolveExpression expr
                return (Stmt (ExpressionStatement expr'))
            resolveBlockItem other = do return other

            resolveExpression :: Expression -> TransM Expression
            resolveExpression (Variable name) = do
                name' <- resolveName name
                return (Variable name')
            resolveExpression (Unary op expr) = do
                expr' <- resolveExpression expr
                return (Unary op expr')
            resolveExpression (Binary Assignment (Variable left) right) = do
                left' <- resolveName left
                right' <- resolveExpression right
                return (Binary Assignment (Variable left') right') 
            resolveExpression (Binary Assignment _ _) = error "assign to non-variable."
            resolveExpression (Binary op left right) = do
                left' <- resolveExpression left
                right' <- resolveExpression right
                return (Binary op left' right') 
            resolveExpression other = do return other
                











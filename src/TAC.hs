module TAC 
    ( translate
    , Program(..)
    , Function(..)
    , Instruction(..)
    , Value(..)
    )
where

import Control.Monad.State

import qualified Parser as P
import Parser (UnaryOperator)

data Program
    = Program Function  
    deriving (Show)

data Function
    = Function String [Instruction]
    deriving (Show)

data Instruction
    = Return Value
    | Unary UnaryOperator Value Value
    deriving (Show)

newtype VarId = VarId Int deriving (Show, Eq)

data Value
    = Constant Int
    | Variable VarId (Maybe String)
    deriving (Show)

data TransState = TransState { nextID :: Int }

type TransM a = State TransState a -- the translation monad encapsulating the translation state


newId :: TransM VarId
newId = do
    state <- get
    let n = nextID state
    put TransState {nextID = (n + 1)}
    return (VarId n)

translate :: P.Program -> Program
translate program = evalState (translateProgram program) initState
  where
    initState = TransState { nextID = 1001 }


translateProgram :: P.Program -> TransM Program
translateProgram (P.Program fun) = do 
    fun' <- translateFunction fun
    return (Program fun')

translateFunction :: P.Function -> TransM Function
translateFunction (P.Function name stmts) = do
    instructions <- concat <$> mapM translateStatement stmts
    return (Function name instructions)

translateStatement :: P.Statement -> TransM [Instruction]
translateStatement (P.ReturnStatement expression) = do
    (instructions, value) <- translateExpression expression
    return (instructions ++ [Return value])

translateExpression :: P.Expression -> TransM ([Instruction], Value)
translateExpression (P.Constant c) = do
    return ([], Constant c)
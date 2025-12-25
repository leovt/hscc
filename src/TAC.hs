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
import Parser 
    ( UnaryOperator
    , BinaryOperator
    )

data Program
    = Program Function  
    deriving (Show)

data Function
    = Function String [Instruction]
    deriving (Show)

data Instruction
    = Return Value
    | Unary UnaryOperator Value Value
    | Binary BinaryOperator Value Value Value
    | Copy Value Value
    | Jump String
    | JumpIfZero String Value
    | JumpIfNotZero String Value
    | Label String
    deriving (Show)

data Value
    = Constant Int
    | Variable String
    deriving (Show)

data TransState = TransState { nextID :: Int }

type TransM a = State TransState a -- the translation monad encapsulating the translation state


newId :: String -> TransM String
newId prefix = do
    state <- get
    let n = nextID state
    put TransState {nextID = n + 1}
    return $ prefix ++ "." ++ show n

translate :: P.Program -> Int -> Program
translate program nextID' = evalState (translateProgram program) initState
    where
        initState = TransState { nextID = nextID' }

        translateProgram :: P.Program -> TransM Program
        translateProgram (P.Program fun) = do 
            fun' <- translateFunction fun
            return (Program fun')

        translateFunction :: P.Function -> TransM Function
        translateFunction (P.Function name items) = do
            instructions <- concat <$> mapM translateBlockItem items
            return (Function name (instructions ++ [Return (Constant 0)]))

        translateBlockItem :: P.BlockItem -> TransM [Instruction]
        translateBlockItem (P.Stmt s) = translateStatement s
        translateBlockItem (P.Decl (P.VariableDeclaration name (Just expr))) = do
            (instr, _value) <- translateExpression (P.Binary P.Assignment (P.Variable name) expr)
            return instr
        translateBlockItem (P.Decl (P.VariableDeclaration _ _)) = return []

        translateStatement :: P.Statement -> TransM [Instruction]
        translateStatement (P.ReturnStatement expression) = do
            (instructions, value) <- translateExpression expression
            return (instructions ++ [Return value])
        translateStatement (P.ExpressionStatement expression) = do
            (instructions, _value) <- translateExpression expression
            return instructions
        translateStatement P.NullStatement = return []

        translateExpression :: P.Expression -> TransM ([Instruction], Value)
        translateExpression (P.Constant c) = do
            return ([], Constant c)

        translateExpression (P.Unary op expression) = do
            (instructions, value) <- translateExpression expression
            varid <- newId "tmp"
            let destination = Variable varid
            return (instructions ++ [Unary op value destination], destination)

        translateExpression (P.Binary P.LogicAnd left right) = do
            (l_instructions, left') <- translateExpression left
            (r_instructions, right') <- translateExpression right
            varid <- newId "tmp"
            false_label <- newId "false.label"
            end_label <- newId "end.label"
            let destination = Variable varid
            let instructions = 
                    l_instructions ++
                    [ JumpIfZero false_label left' ] ++
                    r_instructions ++
                    [ JumpIfZero false_label right'
                    , Copy (Constant 1) destination
                    , Jump end_label
                    , Label false_label
                    , Copy (Constant 0) destination
                    , Label end_label ]
            return (instructions, destination)
            
        translateExpression (P.Binary P.LogicOr left right) = do
            (l_instructions, left') <- translateExpression left
            (r_instructions, right') <- translateExpression right
            varid <- newId "tmp"
            true_label <- newId "true.label"
            end_label <- newId "end.label"
            let destination = Variable varid
            let instructions = 
                    l_instructions ++
                    [JumpIfNotZero true_label left'] ++
                    r_instructions ++
                    [JumpIfNotZero true_label right', 
                    Copy (Constant 0) destination,
                    Jump end_label,
                    Label true_label,
                    Copy (Constant 1) destination,
                    Label end_label]
            return (instructions, destination)

        translateExpression (P.Binary P.Assignment (P.Variable left) right) = do
            (r_instructions, right') <- translateExpression right
            return (r_instructions ++ [Copy right' (Variable left)], right')

        translateExpression (P.Binary P.Assignment _ _) = error "assign to non-variable."

        translateExpression (P.Binary op left right) = do
            (l_instructions, left') <- translateExpression left
            (r_instructions, right') <- translateExpression right
            varid <- newId "tmp"
            let destination = Variable varid
            return (l_instructions ++ r_instructions ++ [Binary op left' right' destination], destination)

        translateExpression (P.Variable name) = do 
            return ([], Variable name)


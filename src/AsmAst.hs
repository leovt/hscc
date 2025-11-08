module AsmAst 
    ( translateProgram
    , emitProgram
    )
where

import qualified Parser as P

data Program
    = Program Function  
    deriving (Show)

data Function
    = Function String [Instruction]
    deriving (Show)

data Instruction
    = Mov Operand Operand
    | Ret
    deriving (Show)

data Operand
    = Imm Int
    | Register
    deriving (Show)

translateProgram :: P.Program -> Program
translateProgram (P.Program fun) = Program (translateFunction fun)

translateFunction :: P.Function -> Function
translateFunction (P.Function name stmts) = Function name $ concatMap translateStatement stmts

translateStatement :: P.Statement -> [Instruction]
translateStatement (P.ReturnStatement expression) = [Mov (translateExpression expression) Register, Ret] 

translateExpression :: P.Expression -> Operand
translateExpression (P.Constant n) = Imm n



emitProgram :: Program -> [String]
emitProgram (Program fun) = (emitFunction fun) ++ [".section .note.GNU-stack,\"\",@progbits"]

emitFunction :: Function -> [String]
emitFunction (Function name instructions) = [".globl " ++ name, name ++ ":"] ++ (map emitInstruction instructions)

emitInstruction :: Instruction -> String
emitInstruction (Mov src dst) = "    movl " ++ (emitOperand src) ++ ", " ++ (emitOperand dst)
emitInstruction Ret = "    ret"

emitOperand :: Operand -> String
emitOperand (Imm n) = "$" ++ (show n)
emitOperand Register = "%eax"

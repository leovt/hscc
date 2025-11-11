module AsmAst 
    ( translateProgram
    , emitProgram
    )
where

import qualified TAC as T

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

translateProgram :: T.Program -> Program
translateProgram (T.Program fun) = Program (translateFunction fun)

translateFunction :: T.Function -> Function
translateFunction (T.Function name stmts) = Function name $ concatMap translateInstruction stmts

translateInstruction :: T.Instruction -> [Instruction]
translateInstruction (T.Return value) = [
    Mov (translateValue value) Register,
    Ret]
    

translateValue :: T.Value -> Operand
translateValue (T.Constant c) = Imm c



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

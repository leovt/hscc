module AsmAst 
    ( translateTACtoASM
    , emitProgram
    )
where

import qualified Data.Map
import Control.Monad.State

import qualified TAC as T
import qualified Parser as P
import TAC(VarId(..))
import Parser(UnaryOperator(..), BinaryOperator(..))

data Program
    = Program Function  
    deriving (Show)

data Function
    = Function String [Instruction]
    deriving (Show)

data Instruction
    = Mov Operand Operand
    | Add Operand Operand
    | Sub Operand Operand
    | Mul Operand Operand
    | Div Operand
    | Not Operand
    | Neg Operand
    | AllocateStack Int
    | Ret
    | Cdq
    deriving (Show)

data Operand
    = Imm Int
    | Register Reg
    | Pseudo String
    | Stack Int
    deriving (Show, Eq, Ord)

data Reg
    = AX
    | DX
    | R10
    | R11
    deriving (Show, Eq, Ord)

translateTACtoASM :: T.Program -> Program
translateTACtoASM = fixInstructions . replacePseudo . translateProgram
    where
        translateProgram :: T.Program -> Program
        translateProgram (T.Program fun) = Program (translateFunction fun)

        translateFunction :: T.Function -> Function
        translateFunction (T.Function name stmts) = Function name $ concatMap translateInstruction stmts

        translateInstruction :: T.Instruction -> [Instruction]
        translateInstruction (T.Return value) = [
            Mov (translateValue value) (Register AX),
            Ret]
        translateInstruction (T.Unary Negate src dst) = [
            Mov (translateValue src) (translateValue dst),
            Neg (translateValue dst)]
        translateInstruction (T.Unary Complement src dst) = [
            Mov (translateValue src) (translateValue dst),
            Not (translateValue dst)]    
        translateInstruction (T.Binary P.Add left right dst) = [
            Mov (translateValue left) (translateValue dst),
            AsmAst.Add (translateValue right) (translateValue dst)]    
        translateInstruction (T.Binary Subtract left right dst) = [
            Mov (translateValue left) (translateValue dst),
            Sub (translateValue right) (translateValue dst)]    
        translateInstruction (T.Binary Multiply left right dst) = [
            Mov (translateValue left) (translateValue dst),
            Mul (translateValue right) (translateValue dst)]    
        translateInstruction (T.Binary Divide left right dst) = [
            Mov (translateValue left) (Register AX),
            Cdq,
            Div (translateValue right),
            Mov (Register AX) (translateValue dst)]    
        translateInstruction (T.Binary Remainder left right dst) = [
            Mov (translateValue left) (Register AX),
            Cdq,
            Div (translateValue right),
            Mov (Register DX) (translateValue dst)]    

        translateValue :: T.Value -> Operand
        translateValue (T.Constant c) = Imm c
        translateValue (T.Variable (VarId n) Nothing) = Pseudo $ "tmp." ++ (show n)
        translateValue (T.Variable (VarId n) (Just hint)) = Pseudo $ hint ++ "." ++ (show n)


data TransState = TransState { 
    stackSize :: Int,
    pseudoMap :: Data.Map.Map Operand Operand }

type TransM a = State TransState a -- the translation monad encapsulating the translation state

replacePseudo :: Program -> Program
replacePseudo program = evalState (replacePseudoProg program) (TransState {stackSize=0, pseudoMap=Data.Map.empty})
    where
        replacePseudoOp :: Operand -> TransM Operand
        replacePseudoOp (Pseudo name) = do
            state <- get
            case Data.Map.lookup (Pseudo name) (pseudoMap state) of
                Just existing -> return existing
                Nothing -> do
                    -- Compute a new Stack operand, e.g., Stack n where n = current map size
                    let n = (stackSize state) + 4
                    let new = Stack (-n)
                    put TransState {stackSize=n, pseudoMap = Data.Map.insert (Pseudo name) new (pseudoMap state)}
                    return new
        replacePseudoOp op = return op

        replacePseudoIns :: Instruction -> TransM Instruction
        replacePseudoIns (Mov src dst) = do
            src' <- replacePseudoOp src
            dst' <- replacePseudoOp dst
            return (Mov src' dst')
        replacePseudoIns (AsmAst.Add src dst) = do
            src' <- replacePseudoOp src
            dst' <- replacePseudoOp dst
            return (AsmAst.Add src' dst')
        replacePseudoIns (Sub src dst) = do
            src' <- replacePseudoOp src
            dst' <- replacePseudoOp dst
            return (Sub src' dst')
        replacePseudoIns (Mul src dst) = do
            src' <- replacePseudoOp src
            dst' <- replacePseudoOp dst
            return (Mul src' dst')
        replacePseudoIns (Neg dst) = do
            dst' <- replacePseudoOp dst
            return (Neg dst')
        replacePseudoIns (Not dst) = do
            dst' <- replacePseudoOp dst
            return (Not dst')
        replacePseudoIns (Div src) = do
            src' <- replacePseudoOp src
            return (Div src')
        replacePseudoIns any = return any 

        replacePseudoFun :: Function -> TransM Function
        replacePseudoFun (Function name instructions) = do
            put (TransState {stackSize=0, pseudoMap=Data.Map.empty}) -- start with an empty mapping
            instructions' <- mapM replacePseudoIns instructions
            state <- get
            return (Function name ([AllocateStack (stackSize state)] ++ instructions'))

        replacePseudoProg :: Program -> TransM Program
        replacePseudoProg (Program fun) = do 
            fun' <- replacePseudoFun fun 
            return (Program fun')



fixInstructions :: Program -> Program
fixInstructions (Program fun) = Program (fixInstructionsFun fun)
    where
        fixInstructionsFun :: Function -> Function
        fixInstructionsFun (Function name instructions) = Function name (concatMap fixInstr instructions)

        fixInstr :: Instruction -> [Instruction]
        fixInstr (Mov (Stack a) (Stack b)) = [
            Mov (Stack a) (Register R10),
            Mov (Register R10) (Stack b)]
        fixInstr (AsmAst.Add (Stack a) (Stack b)) = [
            Mov (Stack a) (Register R10),
            AsmAst.Add (Register R10) (Stack b)]
        fixInstr (Sub (Stack a) (Stack b)) = [
            Mov (Stack a) (Register R10),
            Sub (Register R10) (Stack b)]
        fixInstr (Mul src (Stack b)) = [
            Mov (Stack b) (Register R11),
            Mul src (Register R11),
            Mov (Register R11) (Stack b)]
        fixInstr (Div (Imm n)) = [
            Mov (Imm n) (Register R10),
            Div (Register R10)]
        fixInstr ins = [ins]



emitProgram :: Program -> [String]
emitProgram (Program fun) = (emitFunction fun) ++ [".section .note.GNU-stack,\"\",@progbits"]
    where
        emitFunction :: Function -> [String]
        emitFunction (Function name instructions) = [".globl " ++ name, name ++ ":", "    pushq %rbp", "    movq %rsp, %rbp"] ++ (map emitInstruction instructions)

        emitInstruction :: Instruction -> String
        emitInstruction (Mov src dst) = "    movl " ++ (emitOperand src) ++ ", " ++ (emitOperand dst)
        emitInstruction (AsmAst.Add src dst) = "    addl " ++ (emitOperand src) ++ ", " ++ (emitOperand dst)
        emitInstruction (Sub src dst) = "    subl " ++ (emitOperand src) ++ ", " ++ (emitOperand dst)
        emitInstruction (Mul src dst) = "    imull " ++ (emitOperand src) ++ ", " ++ (emitOperand dst)
        emitInstruction (Div src) = "    idivl " ++ (emitOperand src)
        emitInstruction (Neg dst) = "    negl " ++ (emitOperand dst)
        emitInstruction (Not dst) = "    notl " ++ (emitOperand dst)
        emitInstruction (AllocateStack n) = "    subq $" ++ (show n) ++", %rsp"
        emitInstruction Ret = "    movq %rbp, %rsp\n    popq %rbp\n    ret"
        emitInstruction Cdq = "    cdq"

        emitOperand :: Operand -> String
        emitOperand (Imm n) = "$" ++ (show n)
        emitOperand (Register AX) = "%eax"
        emitOperand (Register DX) = "%edx"
        emitOperand (Register R10) = "%r10d"
        emitOperand (Register R11) = "%r11d"
        emitOperand (Stack n) = (show n)++"(%rbp)"
        emitOperand (Pseudo name) = error $ "emitOperand: unexpected Pseudo operand: " ++ name


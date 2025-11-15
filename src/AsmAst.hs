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
    = TwoOp TwoOperandInstruction Operand Operand
    | OneOp OneOperandInstruction Operand
    | AllocateStack Int
    | Jmp String
    | JmpCC Condition String
    | SetCC Condition Operand
    | Label String
    | Ret
    | Cdq
    deriving (Show)

data OneOperandInstruction
    = Div
    | Not
    | Neg
    deriving (Show)

data TwoOperandInstruction
    = Mov
    | Add
    | Sub
    | Cmp
    | Mul
    | And
    | Or
    | Xor
    | ShLeft
    | ShRight
    deriving (Show)

data Condition = E | NE | G | GE | L | LE
    deriving (Show)

data Operand
    = Imm Int
    | Register Reg
    | Pseudo String
    | Stack Int
    deriving (Show, Eq, Ord)

data Binop
    = Arithmetic TwoOperandInstruction
    | Relational Condition

data Reg
    = AX
    | CX
    | CL -- Todo remove when types are implemented
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
            TwoOp Mov (translateValue value) (Register AX),
            Ret]
        translateInstruction (T.Unary LogicNot src dst) = [
            TwoOp Cmp (Imm 0) (translateValue src),
            TwoOp Mov (Imm 0) (translateValue dst),
            SetCC E (translateValue dst)]
        translateInstruction (T.Unary op src dst) = [
            TwoOp Mov (translateValue src) (translateValue dst),
            OneOp (translateUnary op) (translateValue dst)]
        translateInstruction (T.Binary Divide left right dst) = [
            TwoOp Mov (translateValue left) (Register AX),
            Cdq,
            OneOp Div (translateValue right),
            TwoOp Mov (Register AX) (translateValue dst)]    
        translateInstruction (T.Binary Remainder left right dst) = [
            TwoOp Mov (translateValue left) (Register AX),
            Cdq,
            OneOp Div (translateValue right),
            TwoOp Mov (Register DX) (translateValue dst)]
        translateInstruction (T.Binary op left right dst) = 
            case (translateBinary op) of 
                Arithmetic instruction -> [
                    TwoOp Mov (translateValue left) (translateValue dst),
                    TwoOp instruction (translateValue right) (translateValue dst)]    
                Relational condition -> let dest = (translateValue dst) in [
                    TwoOp Cmp (translateValue right) (translateValue left),
                    TwoOp Mov (Imm 0) dest,
                    SetCC condition dest]
        translateInstruction (T.Copy src dst) = [
            TwoOp Mov (translateValue src) (translateValue dst)]
        translateInstruction (T.Jump (VarId n)) = [
            Jmp $ "tmp." ++ (show n)]
        translateInstruction (T.Label (VarId n)) = [
            Label $ "tmp." ++ (show n)]
        translateInstruction (T.JumpIfZero (VarId n) value) = [
            TwoOp Cmp (Imm 0) (translateValue value),
            JmpCC E $ "tmp." ++ (show n)]
        translateInstruction (T.JumpIfNotZero (VarId n) value) = [
            TwoOp Cmp (Imm 0) (translateValue value),
            JmpCC NE $ "tmp." ++ (show n)]
        
        translateUnary :: UnaryOperator -> OneOperandInstruction
        translateUnary Complement = Not
        translateUnary Negate = Neg    

        translateBinary :: BinaryOperator -> Binop
        translateBinary P.Add = Arithmetic AsmAst.Add
        translateBinary Subtract = Arithmetic Sub
        translateBinary Multiply = Arithmetic Mul
        translateBinary Divide = error $ "Divide does not translate to a two operand form."
        translateBinary Remainder = error $ "Divide does not translate to a two operand form."
        translateBinary BitAnd = Arithmetic And
        translateBinary BitOr = Arithmetic Or
        translateBinary BitXor = Arithmetic Xor
        translateBinary ShiftLeft = Arithmetic ShLeft
        translateBinary ShiftRight = Arithmetic ShRight
        translateBinary Equal = Relational E
        translateBinary NotEqual = Relational NE
        translateBinary Less = Relational L
        translateBinary Greater = Relational G
        translateBinary LessOrEqual = Relational LE
        translateBinary GreaterOrEqual = Relational GE

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
        replacePseudoIns (TwoOp op src dst) = do
            src' <- replacePseudoOp src
            dst' <- replacePseudoOp dst
            return (TwoOp op  src' dst')
        replacePseudoIns (OneOp op dst) = do
            dst' <- replacePseudoOp dst
            return (OneOp op dst')
        replacePseudoIns (SetCC condition dst) = do
            dst' <- replacePseudoOp dst
            return (SetCC condition dst')
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
        fixInstr (TwoOp Mul src (Stack b)) = [
            TwoOp Mov (Stack b) (Register R11),
            TwoOp Mul src (Register R11),
            TwoOp Mov (Register R11) (Stack b)]
        fixInstr (TwoOp ShLeft (Imm n) dst) = [(TwoOp ShLeft (Imm n) dst)]
        fixInstr (TwoOp ShLeft src dst) = [
            TwoOp Mov src (Register CX),
            TwoOp ShLeft (Register CL) (dst)]
        fixInstr (TwoOp ShRight (Imm n) dst) = [(TwoOp ShRight (Imm n) dst)]
        fixInstr (TwoOp ShRight src dst) = [
            TwoOp Mov src (Register CX),
            TwoOp ShRight (Register CL) (dst)]
        fixInstr (TwoOp Cmp src (Imm n)) = [
            TwoOp Mov (Imm n) (Register R11),
            TwoOp Cmp src (Register R11)]
        fixInstr (TwoOp op (Stack a) (Stack b)) = [
            TwoOp Mov (Stack a) (Register R10),
            TwoOp op (Register R10) (Stack b)]
        fixInstr (OneOp Div (Imm n)) = [
            TwoOp Mov (Imm n) (Register R10),
            OneOp Div (Register R10)]
        fixInstr ins = [ins]



emitProgram :: Program -> [String]
emitProgram (Program fun) = (emitFunction fun) ++ [".section .note.GNU-stack,\"\",@progbits"]
    where
        emitFunction :: Function -> [String]
        emitFunction (Function name instructions) = [".globl " ++ name, name ++ ":", "    pushq %rbp", "    movq %rsp, %rbp"] ++ (map emitInstruction instructions)

        emitInstruction :: Instruction -> String
        emitInstruction (TwoOp op src dst) = "    " ++ (twoOp op) ++ " " ++ (emitOperand src) ++ ", " ++ (emitOperand dst)
        emitInstruction (OneOp op src)     = "    " ++ (oneOp op) ++ " " ++ (emitOperand src)
        emitInstruction (AllocateStack n) = "    subq $" ++ (show n) ++", %rsp"
        emitInstruction Ret = "    movq %rbp, %rsp\n    popq %rbp\n    ret"
        emitInstruction Cdq = "    cdq"
        emitInstruction (Jmp label) = "    jmp " ++ label
        emitInstruction (Label label) = label ++ ":"
        emitInstruction (JmpCC condition label) = "    j" ++ (cond condition) ++ " " ++ label
        emitInstruction (SetCC condition dst) = "    set" ++ (cond condition) ++ " " ++ (emitOperand dst)

        twoOp :: TwoOperandInstruction -> String
        twoOp Mov = "movl"
        twoOp AsmAst.Add = "addl"
        twoOp Sub = "subl"
        twoOp Mul = "imull"
        twoOp And = "andl"
        twoOp Or  = "orl"
        twoOp Xor = "xorl"
        twoOp ShLeft = "sall"
        twoOp ShRight = "sarl"
        twoOp Cmp = "cmpl"

        oneOp :: OneOperandInstruction -> String
        oneOp Div = "idivl"
        oneOp Neg = "negl"
        oneOp Not = "notl"

        cond :: Condition -> String
        cond E = "e"
        cond NE = "ne"
        cond G = "g"
        cond GE = "ge"
        cond L = "l"
        cond LE = "le"

        emitOperand :: Operand -> String
        emitOperand (Imm n) = "$" ++ (show n)
        emitOperand (Register AX) = "%eax"
        emitOperand (Register CX) = "%ecx"
        emitOperand (Register CL) = "%cl"
        emitOperand (Register DX) = "%edx"
        emitOperand (Register R10) = "%r10d"
        emitOperand (Register R11) = "%r11d"
        emitOperand (Stack n) = (show n)++"(%rbp)"
        emitOperand (Pseudo name) = error $ "emitOperand: unexpected Pseudo operand: " ++ name


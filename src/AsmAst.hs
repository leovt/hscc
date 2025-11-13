module AsmAst 
    ( translateTACtoASM
    , emitProgram
    )
where

import qualified Data.Map
import Control.Monad.State

import qualified TAC as T
import TAC(VarId(..))
import Parser(UnaryOperator(..))

data Program
    = Program Function  
    deriving (Show)

data Function
    = Function String [Instruction]
    deriving (Show)

data Instruction
    = Mov Operand Operand
    | Not Operand
    | Neg Operand
    | AllocateStack Int
    | Ret
    deriving (Show)

data Operand
    = Imm Int
    | Register Reg
    | Pseudo String
    | Stack Int
    deriving (Show, Eq, Ord)

data Reg
    = AX
    | R10
    deriving (Show, Eq, Ord)

translateTACtoASM :: T.Program -> Program
translateTACtoASM = fixInstructions . replacePseudo . translateProgram


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

translateValue :: T.Value -> Operand
translateValue (T.Constant c) = Imm c
translateValue (T.Variable (VarId n) Nothing) = Pseudo $ "tmp." ++ (show n)
translateValue (T.Variable (VarId n) (Just hint)) = Pseudo $ hint ++ "." ++ (show n)


data TransState = TransState { stackSize :: Int,
                    pseudoMap :: Data.Map.Map Operand Operand }

type TransM a = State TransState a -- the translation monad encapsulating the translation state
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
replacePseudoIns (Neg dst) = do
    dst' <- replacePseudoOp dst
    return (Neg dst')
replacePseudoIns (Not dst) = do
    dst' <- replacePseudoOp dst
    return (Not dst')
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

replacePseudo :: Program -> Program
replacePseudo program = evalState (replacePseudoProg program) (TransState {stackSize=0, pseudoMap=Data.Map.empty})


fixInstructions :: Program -> Program
fixInstructions (Program fun) = Program (fixInstructionsFun fun)

fixInstructionsFun :: Function -> Function
fixInstructionsFun (Function name instructions) = Function name (concatMap fixInstr instructions)

fixInstr :: Instruction -> [Instruction]
fixInstr (Mov (Stack a) (Stack b)) = [
    Mov (Stack a) (Register R10),
    Mov (Register R10) (Stack b)]
fixInstr ins = [ins]



emitProgram :: Program -> [String]
emitProgram (Program fun) = (emitFunction fun) ++ [".section .note.GNU-stack,\"\",@progbits"]

emitFunction :: Function -> [String]
emitFunction (Function name instructions) = [".globl " ++ name, name ++ ":", "    pushq %rbp", "    movq %rsp, %rbp"] ++ (map emitInstruction instructions)

emitInstruction :: Instruction -> String
emitInstruction (Mov src dst) = "    movl " ++ (emitOperand src) ++ ", " ++ (emitOperand dst)
emitInstruction (Neg dst) = "    negl " ++ (emitOperand dst)
emitInstruction (Not dst) = "    notl " ++ (emitOperand dst)
emitInstruction (AllocateStack n) = "    subq $" ++ (show n) ++", %rsp"
emitInstruction Ret = "    movq %rbp, %rsp\n    popq %rbp\n    ret"

emitOperand :: Operand -> String
emitOperand (Imm n) = "$" ++ (show n)
emitOperand (Register AX) = "%eax"
emitOperand (Register R10) = "%r10d"
emitOperand (Stack n) = (show n)++"(%rbp)"
emitOperand (Pseudo name) = error $ "emitOperand: unexpected Pseudo operand: " ++ name


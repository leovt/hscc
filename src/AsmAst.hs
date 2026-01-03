module AsmAst
  ( translateTACtoASM,
    emitProgram,
    Program (..),
    Function (..),
    Instruction (..),
  )
where

import Control.Monad.State
import qualified Data.Map
import Parser (BinaryOperator (..), UnaryOperator (..))
import qualified Parser as P
import qualified TAC as T

{- HLINT ignore "Use newtype instead of data" -}
data Program
  = Program [Function]
  deriving (Show)

data Function
  = Function String [Instruction]
  deriving (Show)

data Instruction
  = TwoOp TwoOperandInstruction Operand Operand
  | OneOp OneOperandInstruction Operand
  | AllocateStack Int
  | DeallocateStack Int
  | Jmp String
  | JmpCC Condition String
  | SetCC Condition Operand
  | Label String
  | Push Operand
  | Call String
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
  | DX
  | BX
  | SP
  | BP
  | SI
  | DI
  | R8
  | R9
  | R10
  | R11
  deriving (Show, Eq, Ord)

data RegSize
  = Reg1
  | Reg4
  | Reg8

translateTACtoASM :: T.Program -> Program
translateTACtoASM = fixInstructions . replacePseudo . translateProgram
  where
    translateProgram :: T.Program -> Program
    translateProgram (T.Program functions) = Program (map translateFunction functions)

    translateFunction :: T.Function -> Function
    translateFunction (T.Function name params stmts) = Function name instructions
      where
        instructions =
          copyRegisterParameters
            ++ copyStackParameters
            ++ body_instructions
            ++ cleanupStack

        params' = map translateValue params

        argRegisters :: [Reg]
        argRegisters = [DI, SI, DX, CX, R8, R9]
        (registerArgs, stackArgs) = splitAt (length argRegisters) params'
        copyRegisterParameters = zipWith movarg registerArgs argRegisters
        copyStackParameters = zipWith movstk stackArgs [16, 24 ..]

        movarg :: Operand -> Reg -> Instruction
        movarg arg reg = TwoOp Mov (Register reg) arg

        movstk :: Operand -> Int -> Instruction
        movstk arg offset = TwoOp Mov (Stack offset) arg

        body_instructions = concatMap translateInstruction stmts
        cleanupStack = []

    translateInstruction :: T.Instruction -> [Instruction]
    translateInstruction (T.Return value) =
      [ TwoOp Mov (translateValue value) (Register AX),
        Ret
      ]
    translateInstruction (T.Unary LogicNot src dst) =
      [ TwoOp Cmp (Imm 0) (translateValue src),
        TwoOp Mov (Imm 0) (translateValue dst),
        SetCC E (translateValue dst)
      ]
    translateInstruction (T.Unary op src dst) =
      [ TwoOp Mov (translateValue src) (translateValue dst),
        OneOp (translateUnary op) (translateValue dst)
      ]
    translateInstruction (T.Binary Divide left right dst) =
      [ TwoOp Mov (translateValue left) (Register AX),
        Cdq,
        OneOp Div (translateValue right),
        TwoOp Mov (Register AX) (translateValue dst)
      ]
    translateInstruction (T.Binary Remainder left right dst) =
      [ TwoOp Mov (translateValue left) (Register AX),
        Cdq,
        OneOp Div (translateValue right),
        TwoOp Mov (Register DX) (translateValue dst)
      ]
    translateInstruction (T.Binary op left right dst) =
      case translateBinary op of
        Arithmetic instruction ->
          [ TwoOp Mov (translateValue left) (translateValue dst),
            TwoOp instruction (translateValue right) (translateValue dst)
          ]
        Relational condition ->
          let dest = translateValue dst
           in [ TwoOp Cmp (translateValue right) (translateValue left),
                TwoOp Mov (Imm 0) dest,
                SetCC condition dest
              ]
    translateInstruction (T.Copy src dst) =
      [ TwoOp Mov (translateValue src) (translateValue dst)
      ]
    translateInstruction (T.Jump label) = [Jmp label]
    translateInstruction (T.Label label) = [Label label]
    translateInstruction (T.JumpIfZero label value) =
      [ TwoOp Cmp (Imm 0) (translateValue value),
        JmpCC E label
      ]
    translateInstruction (T.JumpIfNotZero label value) =
      [ TwoOp Cmp (Imm 0) (translateValue value),
        JmpCC NE label
      ]
    translateInstruction (T.FunctionCall name args value) =
      allocateStackSpace
        ++ passRegisterArguments
        ++ passStackArguments
        ++ [Call name]
        ++ deallocateStackSpace
        ++ saveReturnValue
      where
        argRegisters :: [Reg]
        argRegisters = [DI, SI, DX, CX, R8, R9]
        (registerArgs, stackArgs) = splitAt (length argRegisters) args
        stackPadding = 8 * mod (length stackArgs) 2
        cleanupSize = stackPadding + 8 * length stackArgs
        {- HLINT ignore "Use list comprehension" -}
        allocateStackSpace = if stackPadding == 0 then [] else [AllocateStack stackPadding]
        deallocateStackSpace = if cleanupSize == 0 then [] else [DeallocateStack cleanupSize]
        passRegisterArguments = zipWith movarg registerArgs argRegisters
        passStackArguments = concatMap (movstk . translateValue) (reverse stackArgs)
        saveReturnValue = [TwoOp Mov (Register AX) (translateValue value)]

        movarg :: T.Value -> Reg -> Instruction
        movarg val reg = TwoOp Mov (translateValue val) (Register reg)

        movstk :: Operand -> [Instruction]
        movstk op@(Imm _) = [Push op]
        movstk op@(Register _) = [Push op]
        movstk op = [TwoOp Mov op (Register AX), Push (Register AX)]

    translateUnary :: UnaryOperator -> OneOperandInstruction
    translateUnary Complement = Not
    translateUnary Negate = Neg
    translateUnary LogicNot = error "LogicNot does not translate to a one operand form."
    translateUnary PreIncrement = error "PreIncrement does not translate to a one operand form."
    translateUnary PreDecrement = error "PreDecrement does not translate to a one operand form."
    translateUnary PostIncrement = error "PostIncrement does not translate to a one operand form."
    translateUnary PostDecrement = error "PostDecrement does not translate to a one operand form."

    translateBinary :: BinaryOperator -> Binop
    translateBinary P.Add = Arithmetic AsmAst.Add
    translateBinary Subtract = Arithmetic Sub
    translateBinary Multiply = Arithmetic Mul
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
    translateBinary Divide = error "Divide does not translate to a two operand form."
    translateBinary Remainder = error "Remainder does not translate to a two operand form."
    translateBinary LogicAnd = error "LogicAnd does not translate to a two operand form."
    translateBinary LogicOr = error "LogicOr does not translate to a two operand form."
    translateBinary Assignment = error "Assignment does not translate to a two operand form."
    translateBinary (CompoundAssignment _) = error "CompoundAssignment does not translate to a two operand form."

    translateValue :: T.Value -> Operand
    translateValue (T.Constant c) = Imm c
    translateValue (T.Variable name) = Pseudo name

data TransState = TransState
  { stackSize :: Int,
    pseudoMap :: Data.Map.Map Operand Operand
  }

type TransM a = State TransState a -- the translation monad encapsulating the translation state

replacePseudo :: Program -> Program
replacePseudo program = evalState (replacePseudoProg program) (TransState {stackSize = 0, pseudoMap = Data.Map.empty})
  where
    replacePseudoOp :: Operand -> TransM Operand
    replacePseudoOp (Pseudo name) = do
      state <- get
      case Data.Map.lookup (Pseudo name) (pseudoMap state) of
        Just existing -> return existing
        Nothing -> do
          -- Compute a new Stack operand, e.g., Stack n where n = current map size
          let n = stackSize state + 4
          let new = Stack (-n)
          put TransState {stackSize = n, pseudoMap = Data.Map.insert (Pseudo name) new (pseudoMap state)}
          return new
    replacePseudoOp op = return op

    replacePseudoIns :: Instruction -> TransM Instruction
    replacePseudoIns (TwoOp op src dst) = do
      src' <- replacePseudoOp src
      dst' <- replacePseudoOp dst
      return (TwoOp op src' dst')
    replacePseudoIns (OneOp op dst) = do
      dst' <- replacePseudoOp dst
      return (OneOp op dst')
    replacePseudoIns (SetCC condition dst) = do
      dst' <- replacePseudoOp dst
      return (SetCC condition dst')
    replacePseudoIns (Push src) = do
      src' <- replacePseudoOp src
      return (Push src')
    replacePseudoIns any = return any

    replacePseudoFun :: Function -> TransM Function
    replacePseudoFun (Function name instructions) = do
      put (TransState {stackSize = 0, pseudoMap = Data.Map.empty}) -- start with an empty mapping
      instructions' <- mapM replacePseudoIns instructions
      state <- get
      let size = 16 * quot (stackSize state + 15) 16
      return (Function name (AllocateStack size : instructions'))

    replacePseudoProg :: Program -> TransM Program
    replacePseudoProg (Program functions) = do
      functions' <- mapM replacePseudoFun functions
      return (Program functions')

fixInstructions :: Program -> Program
fixInstructions (Program fun) = Program (map fixInstructionsFun fun)
  where
    fixInstructionsFun :: Function -> Function
    fixInstructionsFun (Function name instructions) = Function name (concatMap fixInstr instructions)

    fixInstr :: Instruction -> [Instruction]
    fixInstr (TwoOp Mul src (Stack b)) =
      [ TwoOp Mov (Stack b) (Register R11),
        TwoOp Mul src (Register R11),
        TwoOp Mov (Register R11) (Stack b)
      ]
    fixInstr (TwoOp ShLeft (Imm n) dst) = [TwoOp ShLeft (Imm n) dst]
    fixInstr (TwoOp ShLeft src dst) =
      [ TwoOp Mov src (Register CX),
        TwoOp ShLeft (Register CX) dst
      ]
    fixInstr (TwoOp ShRight (Imm n) dst) = [TwoOp ShRight (Imm n) dst]
    fixInstr (TwoOp ShRight src dst) =
      [ TwoOp Mov src (Register CX),
        TwoOp ShRight (Register CX) dst
      ]
    fixInstr (TwoOp Cmp src (Imm n)) =
      [ TwoOp Mov (Imm n) (Register R11),
        TwoOp Cmp src (Register R11)
      ]
    fixInstr (TwoOp op (Stack a) (Stack b)) =
      [ TwoOp Mov (Stack a) (Register R10),
        TwoOp op (Register R10) (Stack b)
      ]
    fixInstr (OneOp Div (Imm n)) =
      [ TwoOp Mov (Imm n) (Register R10),
        OneOp Div (Register R10)
      ]
    fixInstr ins = [ins]

emitProgram :: Program -> [String]
emitProgram (Program fun) = concatMap emitFunction fun ++ [".section .note.GNU-stack,\"\",@progbits"]
  where
    emitFunction :: Function -> [String]
    emitFunction (Function name instructions) = [".globl " ++ name, name ++ ":", "    pushq %rbp", "    movq %rsp, %rbp"] ++ map emitInstruction instructions

    emitInstruction :: Instruction -> String
    emitInstruction (TwoOp ShLeft src dst) = "    " ++ twoOp ShLeft ++ " " ++ emitOperand Reg1 src ++ ", " ++ emitOperand Reg4 dst
    emitInstruction (TwoOp ShRight src dst) = "    " ++ twoOp ShRight ++ " " ++ emitOperand Reg1 src ++ ", " ++ emitOperand Reg4 dst
    emitInstruction (TwoOp op src dst) = "    " ++ twoOp op ++ " " ++ emitOperand Reg4 src ++ ", " ++ emitOperand Reg4 dst
    emitInstruction (OneOp op src) = "    " ++ oneOp op ++ " " ++ emitOperand Reg4 src
    emitInstruction (AllocateStack n) = "    subq $" ++ show n ++ ", %rsp"
    emitInstruction (DeallocateStack n) = "    addq $" ++ show n ++ ", %rsp"
    emitInstruction Ret = "    movq %rbp, %rsp\n    popq %rbp\n    ret"
    emitInstruction Cdq = "    cdq"
    emitInstruction (Jmp label) = "    jmp " ++ label
    emitInstruction (Label label) = label ++ ":"
    emitInstruction (JmpCC condition label) = "    j" ++ cond condition ++ " " ++ label
    emitInstruction (SetCC condition dst) = "    set" ++ cond condition ++ " " ++ emitOperand Reg4 dst
    emitInstruction (Push src) = "    pushq " ++ emitOperand Reg8 src
    emitInstruction (Call name) = "    call " ++ name

    twoOp :: TwoOperandInstruction -> String
    twoOp Mov = "movl"
    twoOp AsmAst.Add = "addl"
    twoOp Sub = "subl"
    twoOp Mul = "imull"
    twoOp And = "andl"
    twoOp Or = "orl"
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

    emitOperand :: RegSize -> Operand -> String
    emitOperand _ (Imm n) = "$" ++ show n
    emitOperand _ (Stack n) = show n ++ "(%rbp)"
    emitOperand _ (Pseudo name) = error $ "emitOperand: unexpected Pseudo operand: " ++ name
    emitOperand Reg1 (Register AX) = "%al"
    emitOperand Reg1 (Register CX) = "%cl"
    emitOperand Reg1 (Register DX) = "%dl"
    emitOperand Reg1 (Register BX) = "%bl"
    emitOperand Reg1 (Register SP) = "%spl"
    emitOperand Reg1 (Register BP) = "%bpl"
    emitOperand Reg1 (Register SI) = "%sil"
    emitOperand Reg1 (Register DI) = "%dil"
    emitOperand Reg1 (Register R8) = "%r8b"
    emitOperand Reg1 (Register R9) = "%r9b"
    emitOperand Reg1 (Register R10) = "%r10b"
    emitOperand Reg1 (Register R11) = "%r11b"
    emitOperand Reg4 (Register AX) = "%eax"
    emitOperand Reg4 (Register CX) = "%ecx"
    emitOperand Reg4 (Register DX) = "%edx"
    emitOperand Reg4 (Register BX) = "%ebx"
    emitOperand Reg4 (Register SP) = "%esp"
    emitOperand Reg4 (Register BP) = "%ebp"
    emitOperand Reg4 (Register SI) = "%esi"
    emitOperand Reg4 (Register DI) = "%edi"
    emitOperand Reg4 (Register R8) = "%r8d"
    emitOperand Reg4 (Register R9) = "%r9d"
    emitOperand Reg4 (Register R10) = "%r10d"
    emitOperand Reg4 (Register R11) = "%r11d"
    emitOperand Reg8 (Register AX) = "%rax"
    emitOperand Reg8 (Register CX) = "%rcx"
    emitOperand Reg8 (Register DX) = "%rdx"
    emitOperand Reg8 (Register BX) = "%rbx"
    emitOperand Reg8 (Register SP) = "%rsp"
    emitOperand Reg8 (Register BP) = "%rbp"
    emitOperand Reg8 (Register SI) = "%rsi"
    emitOperand Reg8 (Register DI) = "%rdi"
    emitOperand Reg8 (Register R8) = "%r8"
    emitOperand Reg8 (Register R9) = "%r9"
    emitOperand Reg8 (Register R10) = "%r10"
    emitOperand Reg8 (Register R11) = "%r11"
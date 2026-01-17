module AsmAst
  ( translateTACtoASM,
    emitProgram,
    Program (..),
    TopLevel (..),
    Instruction (..),
  )
where

import CTypes (ArithmeticType (..), CType (..), IntSize (..), IntegralType (..), Signed (..), intT)
import Control.Monad.State
import Data.Bits (Bits (shiftL))
import qualified Data.Map
import Parser (BinaryOperator (..), UnaryOperator (..))
import qualified Parser as P
import TAC (valueType)
import qualified TAC as T

{- HLINT ignore "Use newtype instead of data" -}
data Program
  = Program [TopLevel]
  deriving (Show)

data TopLevel
  = Function String Bool [Instruction]
  | StaticVariable AsmType String Bool Integer
  deriving (Show)

data Instruction
  = TwoOp TwoOperandInstruction AsmType Operand Operand
  | OneOp OneOperandInstruction AsmType Operand
  | MovSX Operand Operand
  | MovZX Operand Operand
  | AllocateStack Int
  | DeallocateStack Int
  | Jmp String
  | JmpCC Condition String
  | SetCC Condition Operand
  | Label String
  | Push Operand
  | Call String
  | Ret
  | Cdq AsmType
  deriving (Show)

data OneOperandInstruction
  = Div Signed
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
  | ShRight Signed
  deriving (Show)

data Condition = E | NE | G | GE | L | LE | A | AE | B | BE
  deriving (Show)

data MemoryOperand
  = Stack Int String
  | Data String
  deriving (Show, Eq, Ord)

data Operand
  = Imm Integer
  | Register Reg
  | Pseudo AsmType String
  | Memory MemoryOperand
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

data AsmType
  = Longword
  | Quadword
  deriving (Show, Eq, Ord)

asmType :: CType -> AsmType
asmType (ArithmeticType (Integral (IType _ Int))) = Longword
asmType (ArithmeticType (Integral (IType _ Long))) = Quadword
asmType _ = error "Unsupported CType for AsmType."

asmSign :: CType -> Signed
asmSign (ArithmeticType (Integral (IType s _))) = s
asmSign _ = error "Unsupported CType for asmSign."

asmValueType :: T.Value -> AsmType
asmValueType = asmType . valueType

asmValueSign :: T.Value -> Signed
asmValueSign = asmSign . valueType

translateTACtoASM :: T.Program -> Program
translateTACtoASM = fixImmediates . fixInstructions . replacePseudo . translateProgram
  where
    translateProgram :: T.Program -> Program
    translateProgram (T.Program functions) = Program (map translateFunction functions)

    translateFunction :: T.TopLevel -> TopLevel
    translateFunction (T.Function name global params stmts) = Function name global instructions
      where
        instructions =
          copyRegisterParameters
            ++ copyStackParameters
            ++ body_instructions
            ++ cleanupStack

        argRegisters :: [Reg]
        argRegisters = [DI, SI, DX, CX, R8, R9]
        (registerArgs, stackArgs) = splitAt (length argRegisters) params
        copyRegisterParameters = zipWith movarg registerArgs argRegisters
        copyStackParameters = zipWith movstk stackArgs [16, 24 ..]

        movarg :: T.Value -> Reg -> Instruction
        movarg arg reg = TwoOp Mov (asmValueType arg) (Register reg) (translateValue arg)

        movstk :: T.Value -> Int -> Instruction
        movstk arg@(T.Variable _ _ name) offset = TwoOp Mov (asmValueType arg) (Memory $ Stack offset name) (translateValue arg)
        movstk _ _ = error "Parameters must be variables."

        body_instructions = concatMap translateInstruction stmts
        cleanupStack = []
    translateFunction (T.StaticVariable t name global init) = StaticVariable (asmType t) name global init

    translateInstruction :: T.Instruction -> [Instruction]
    translateInstruction (T.Return value) =
      [ TwoOp Mov (asmValueType value) (translateValue value) (Register AX),
        Ret
      ]
    translateInstruction (T.Unary LogicNot src dst) =
      [ TwoOp Cmp (asmValueType src) (Imm 0) (translateValue src),
        TwoOp Mov (asmType intT) (Imm 0) (translateValue dst),
        SetCC E (translateValue dst)
      ]
    translateInstruction (T.Unary op src dst) =
      [ TwoOp Mov (asmValueType src) (translateValue src) (translateValue dst),
        OneOp (translateUnary op) (asmValueType src) (translateValue dst)
      ]
    translateInstruction (T.Binary Divide left right dst) =
      [ TwoOp Mov (asmValueType left) (translateValue left) (Register AX),
        case asmValueSign left of
          Signed -> Cdq (asmValueType left)
          Unsigned -> TwoOp Mov (asmValueType left) (Imm 0) (Register DX),
        OneOp (Div (asmValueSign right)) (asmValueType right) (translateValue right),
        TwoOp Mov (asmValueType left) (Register AX) (translateValue dst)
      ]
    translateInstruction (T.Binary Remainder left right dst) =
      [ TwoOp Mov (asmValueType left) (translateValue left) (Register AX),
        case asmValueSign left of
          Signed -> Cdq (asmValueType left)
          Unsigned -> TwoOp Mov (asmValueType left) (Imm 0) (Register DX),
        OneOp (Div (asmValueSign right)) (asmValueType right) (translateValue right),
        TwoOp Mov (asmValueType left) (Register DX) (translateValue dst)
      ]
    translateInstruction (T.Binary op left right dst) =
      case translateBinary (asmValueSign left) op of
        Arithmetic instruction ->
          [ TwoOp Mov (asmValueType dst) (translateValue left) (translateValue dst),
            TwoOp instruction (asmValueType dst) (translateValue right) (translateValue dst)
          ]
        Relational condition ->
          let dest = translateValue dst
           in [ TwoOp Cmp (asmValueType right) (translateValue right) (translateValue left),
                TwoOp Mov (asmType intT) (Imm 0) dest,
                SetCC condition dest
              ]
    translateInstruction (T.Copy src dst) =
      [ TwoOp Mov (asmValueType src) (translateValue src) (translateValue dst)
      ]
    translateInstruction (T.Jump label) = [Jmp label]
    translateInstruction (T.Label label) = [Label label]
    translateInstruction (T.JumpIfZero label value) =
      [ TwoOp Cmp (asmValueType value) (Imm 0) (translateValue value),
        JmpCC E label
      ]
    translateInstruction (T.JumpIfNotZero label value) =
      [ TwoOp Cmp (asmValueType value) (Imm 0) (translateValue value),
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
        passStackArguments = concatMap movstk (reverse stackArgs)
        saveReturnValue = [TwoOp Mov (asmValueType value) (Register AX) (translateValue value)]

        movarg :: T.Value -> Reg -> Instruction
        movarg val reg = TwoOp Mov (asmValueType val) (translateValue val) (Register reg)

        movstk :: T.Value -> [Instruction]
        movstk val = case translateValue val of
          op@(Imm _) -> [Push op]
          op@(Register _) -> [Push op]
          op -> [TwoOp Mov (asmValueType val) op (Register AX), Push (Register AX)]
    translateInstruction (T.SignExtend src dst) = [MovSX (translateValue src) (translateValue dst)]
    translateInstruction (T.ZeroExtend src dst) = [MovZX (translateValue src) (translateValue dst)]
    translateInstruction (T.Truncate src dst) = [TwoOp Mov Longword (translateValue src) (translateValue dst)]

    translateUnary :: UnaryOperator -> OneOperandInstruction
    translateUnary Complement = Not
    translateUnary Negate = Neg
    translateUnary LogicNot = error "LogicNot does not translate to a one operand form."
    translateUnary PreIncrement = error "PreIncrement does not translate to a one operand form."
    translateUnary PreDecrement = error "PreDecrement does not translate to a one operand form."
    translateUnary PostIncrement = error "PostIncrement does not translate to a one operand form."
    translateUnary PostDecrement = error "PostDecrement does not translate to a one operand form."

    translateBinary :: Signed -> BinaryOperator -> Binop
    translateBinary _ P.Add = Arithmetic AsmAst.Add
    translateBinary _ Subtract = Arithmetic Sub
    translateBinary _ Multiply = Arithmetic Mul
    translateBinary _ BitAnd = Arithmetic And
    translateBinary _ BitOr = Arithmetic Or
    translateBinary _ BitXor = Arithmetic Xor
    translateBinary _ ShiftLeft = Arithmetic ShLeft
    translateBinary sign ShiftRight = Arithmetic (ShRight sign)
    translateBinary _ Equal = Relational E
    translateBinary _ NotEqual = Relational NE
    translateBinary Signed Less = Relational L
    translateBinary Signed Greater = Relational G
    translateBinary Signed LessOrEqual = Relational LE
    translateBinary Signed GreaterOrEqual = Relational GE
    translateBinary Unsigned Less = Relational B
    translateBinary Unsigned Greater = Relational A
    translateBinary Unsigned LessOrEqual = Relational BE
    translateBinary Unsigned GreaterOrEqual = Relational AE
    translateBinary _ Divide = error "Divide does not translate to a two operand form."
    translateBinary _ Remainder = error "Remainder does not translate to a two operand form."
    translateBinary _ LogicAnd = error "LogicAnd does not translate to a two operand form."
    translateBinary _ LogicOr = error "LogicOr does not translate to a two operand form."
    translateBinary _ Assignment = error "Assignment does not translate to a two operand form."
    translateBinary _ (CompoundAssignment _) = error "CompoundAssignment does not translate to a two operand form."

    translateValue :: T.Value -> Operand
    translateValue (T.Constant _ c) = Imm c
    translateValue (T.Variable t False name) = Pseudo (asmType t) name
    translateValue (T.Variable _ True name) = Memory $ Data name

data TransState = TransState
  { stackSize :: Int,
    pseudoMap :: Data.Map.Map Operand Operand
  }

type TransM a = State TransState a -- the translation monad encapsulating the translation state

replacePseudo :: Program -> Program
replacePseudo program = evalState (replacePseudoProg program) (TransState {stackSize = 0, pseudoMap = Data.Map.empty})
  where
    replacePseudoOp :: Operand -> TransM Operand
    replacePseudoOp operand@(Pseudo t name) = do
      state <- get
      case Data.Map.lookup operand (pseudoMap state) of
        Just existing -> return existing
        Nothing -> do
          -- Compute a new Stack operand, e.g., Stack n where n = current map size
          let sz = case t of
                Longword -> 4
                Quadword -> 8
              n = (stackSize state + sz + sz - 1) `div` sz * sz -- align to sz
          let new = Memory $ Stack (-n) name
          put TransState {stackSize = n, pseudoMap = Data.Map.insert operand new (pseudoMap state)}
          return new
    replacePseudoOp op = return op

    replacePseudoIns :: Instruction -> TransM Instruction
    replacePseudoIns (TwoOp op t src dst) = do
      src' <- replacePseudoOp src
      dst' <- replacePseudoOp dst
      return (TwoOp op t src' dst')
    replacePseudoIns (OneOp op t dst) = do
      dst' <- replacePseudoOp dst
      return (OneOp op t dst')
    replacePseudoIns (SetCC condition dst) = do
      dst' <- replacePseudoOp dst
      return (SetCC condition dst')
    replacePseudoIns (Push src) = do
      src' <- replacePseudoOp src
      return (Push src')
    replacePseudoIns (MovSX src dst) = do
      src' <- replacePseudoOp src
      dst' <- replacePseudoOp dst
      return (MovSX src' dst')
    replacePseudoIns (MovZX src dst) = do
      src' <- replacePseudoOp src
      dst' <- replacePseudoOp dst
      return (MovZX src' dst')
    replacePseudoIns any = return any

    replacePseudoFun :: TopLevel -> TransM TopLevel
    replacePseudoFun (Function name global instructions) = do
      put (TransState {stackSize = 0, pseudoMap = Data.Map.empty}) -- start with an empty mapping
      instructions' <- mapM replacePseudoIns instructions
      state <- get
      let size = 16 * quot (stackSize state + 15) 16
      return (Function name global (AllocateStack size : instructions'))
    replacePseudoFun other = return other

    replacePseudoProg :: Program -> TransM Program
    replacePseudoProg (Program functions) = do
      functions' <- mapM replacePseudoFun functions
      return (Program functions')

fixInstructions :: Program -> Program
fixInstructions (Program fun) = Program (map fixInstructionsFun fun)
  where
    fixInstructionsFun :: TopLevel -> TopLevel
    fixInstructionsFun (Function name global instructions) = Function name global (concatMap fixInstr instructions)
    fixInstructionsFun other = other

    fixInstr :: Instruction -> [Instruction]
    fixInstr (TwoOp Mul t src dst@(Memory _)) =
      [ TwoOp Mov t dst (Register R11),
        TwoOp Mul t src (Register R11),
        TwoOp Mov t (Register R11) dst
      ]
    fixInstr (TwoOp ShLeft t (Imm n) dst) = [TwoOp ShLeft t (Imm n) dst]
    fixInstr (TwoOp ShLeft t src dst) =
      [ TwoOp Mov t src (Register CX),
        TwoOp ShLeft t (Register CX) dst
      ]
    fixInstr (TwoOp (ShRight sign) t (Imm n) dst) = [TwoOp (ShRight sign) t (Imm n) dst]
    fixInstr (TwoOp (ShRight sign) t src dst) =
      [ TwoOp Mov t src (Register CX),
        TwoOp (ShRight sign) t (Register CX) dst
      ]
    fixInstr (TwoOp Cmp t src (Imm n)) =
      [ TwoOp Mov t (Imm n) (Register R11),
        TwoOp Cmp t src (Register R11)
      ]
    fixInstr (TwoOp op t src@(Memory _) dst@(Memory _)) =
      [ TwoOp Mov t src (Register R10),
        TwoOp op t (Register R10) dst
      ]
    fixInstr (OneOp op@(Div _) t (Imm n)) =
      [ TwoOp Mov t (Imm n) (Register R10),
        OneOp op t (Register R10)
      ]
    fixInstr (MovSX src@(Imm _) dst@(Memory _)) =
      [ TwoOp Mov Longword src (Register R10),
        MovSX (Register R10) (Register R11),
        TwoOp Mov Quadword (Register R11) dst
      ]
    fixInstr (MovSX src dst@(Memory _)) =
      [ MovSX src (Register R11),
        TwoOp Mov Quadword (Register R11) dst
      ]
    fixInstr (MovSX src@(Imm _) dst) =
      [ TwoOp Mov Longword src (Register R10),
        MovSX (Register R10) dst
      ]
    fixInstr (MovZX src dst@(Register _)) =
      [ TwoOp Mov Longword src dst
      ]
    fixInstr (MovZX src dst) =
      [ TwoOp Mov Longword src (Register R11),
        TwoOp Mov Quadword (Register R11) dst
      ]
    fixInstr ins = [ins]

fixImmediates :: Program -> Program
fixImmediates (Program p) = Program (map fixImmediatesTop p)
  where
    fixImmediatesTop :: TopLevel -> TopLevel
    fixImmediatesTop (Function name global instructions) = Function name global (concatMap fixInstr instructions)
    fixImmediatesTop other = other

    fixInstr :: Instruction -> [Instruction]
    fixInstr ins@(TwoOp Mov Quadword _ (Register _)) = [ins]
    fixInstr ins@(TwoOp op Quadword src@(Imm n) dst)
      | fitsImm32Signed n = [ins]
      | otherwise =
          [ TwoOp Mov Quadword src (Register R10),
            TwoOp op Quadword (Register R10) dst
          ]
    fixInstr ins@(Push src@(Imm n))
      | fitsImm32Signed n = [ins]
      | otherwise =
          [ TwoOp Mov Quadword src (Register R10),
            Push (Register R10)
          ]
    fixInstr ins = [ins]

    fitsImm32Signed :: Integer -> Bool
    fitsImm32Signed n =
      n >= -(1 `shiftL` 31) && n <= (1 `shiftL` 31) - 1

emitProgram :: Program -> [String]
emitProgram (Program fun) = concatMap emitTopLevel fun ++ [".section .note.GNU-stack,\"\",@progbits"]
  where
    emitTopLevel :: TopLevel -> [String]
    emitTopLevel (Function name global instructions) =
      let asmglobal = if global then [".globl " ++ name] else []
       in asmglobal ++ [name ++ ":", "    pushq %rbp", "    movq %rsp, %rbp"] ++ map emitInstruction instructions
    emitTopLevel (StaticVariable t name global init) =
      let asmglobal = if global then [".globl " ++ name] else []
          alignment = case t of
            Longword -> ".align 4"
            Quadword -> ".align 8"
          dataDirective = case t of
            Longword -> "    .long " ++ show (reduceImm Reg4 init)
            Quadword -> "    .quad " ++ show (reduceImm Reg8 init)
       in asmglobal ++ [".data", alignment, name ++ ":", dataDirective, ".text"]

    emitInstruction :: Instruction -> String
    emitInstruction ins@(TwoOp ShLeft t src dst) = "    " ++ twoOp ShLeft t ++ " " ++ emitOperand Reg1 src ++ ", " ++ emitOperand (regSize t) dst ++ comment ins
    emitInstruction ins@(TwoOp shift@(ShRight _) t src dst) = "    " ++ twoOp shift t ++ " " ++ emitOperand Reg1 src ++ ", " ++ emitOperand (regSize t) dst ++ comment ins
    emitInstruction ins@(TwoOp op t src dst) = "    " ++ twoOp op t ++ " " ++ emitOperand (regSize t) src ++ ", " ++ emitOperand (regSize t) dst ++ comment ins
    emitInstruction (OneOp op t src) = "    " ++ oneOp op t ++ " " ++ emitOperand (regSize t) src
    emitInstruction (AllocateStack n) = "    subq $" ++ show n ++ ", %rsp"
    emitInstruction (DeallocateStack n) = "    addq $" ++ show n ++ ", %rsp"
    emitInstruction Ret = "    movq %rbp, %rsp\n    popq %rbp\n    ret"
    emitInstruction (Cdq Longword) = "    cdq"
    emitInstruction (Cdq Quadword) = "    cqo"
    emitInstruction (Jmp label) = "    jmp " ++ label
    emitInstruction (Label label) = label ++ ":"
    emitInstruction (JmpCC condition label) = "    j" ++ cond condition ++ " " ++ label
    emitInstruction (SetCC condition dst) = "    set" ++ cond condition ++ " " ++ emitOperand Reg4 dst
    emitInstruction (Push src) = "    pushq " ++ emitOperand Reg8 src
    emitInstruction (Call name) = "    call " ++ name
    emitInstruction (MovSX src dst) = "    movslq " ++ emitOperand Reg4 src ++ ", " ++ emitOperand Reg8 dst
    emitInstruction (MovZX _ _) = error "emitInstruction: MovZX not implemented, should be removed in fixInstructions"

    srcComment :: Instruction -> String
    srcComment (TwoOp _ _ src@(Memory (Stack _ name)) _) = " # " ++ emitOperand Reg8 src ++ " = " ++ name
    srcComment _ = ""

    dstComment :: Instruction -> String
    dstComment (TwoOp _ _ _ dst@(Memory (Stack _ name))) = " # " ++ emitOperand Reg8 dst ++ " = " ++ name
    dstComment _ = ""

    comment :: Instruction -> String
    comment ins = srcComment ins ++ dstComment ins

    twoOp :: TwoOperandInstruction -> AsmType -> String
    twoOp Mov t = "mov" ++ typeSuffix t
    twoOp AsmAst.Add t = "add" ++ typeSuffix t
    twoOp Sub t = "sub" ++ typeSuffix t
    twoOp Mul t = "imul" ++ typeSuffix t
    twoOp And t = "and" ++ typeSuffix t
    twoOp Or t = "or" ++ typeSuffix t
    twoOp Xor t = "xor" ++ typeSuffix t
    twoOp ShLeft t = "sal" ++ typeSuffix t
    twoOp (ShRight Signed) t = "sar" ++ typeSuffix t
    twoOp (ShRight Unsigned) t = "shr" ++ typeSuffix t
    twoOp Cmp t = "cmp" ++ typeSuffix t

    oneOp :: OneOperandInstruction -> AsmType -> String
    oneOp (Div Signed) t = "idiv" ++ typeSuffix t
    oneOp (Div Unsigned) t = "div" ++ typeSuffix t
    oneOp Neg t = "neg" ++ typeSuffix t
    oneOp Not t = "not" ++ typeSuffix t

    cond :: Condition -> String
    cond E = "e"
    cond NE = "ne"
    cond G = "g"
    cond GE = "ge"
    cond L = "l"
    cond LE = "le"
    cond A = "a"
    cond AE = "ae"
    cond B = "b"
    cond BE = "be"

    typeSuffix :: AsmType -> String
    typeSuffix Longword = "l"
    typeSuffix Quadword = "q"

    regSize :: AsmType -> RegSize
    regSize Longword = Reg4
    regSize Quadword = Reg8

    emitOperand :: RegSize -> Operand -> String
    emitOperand sz (Imm n) = "$" ++ show (reduceImm sz n)
    emitOperand _ (Memory (Stack n _)) = show n ++ "(%rbp)"
    emitOperand _ (Memory (Data name)) = name ++ "(%rip)"
    emitOperand _ (Pseudo _ name) = error $ "emitOperand: unexpected Pseudo operand: " ++ name
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

    reduceImm :: RegSize -> Integer -> Integer
    reduceImm Reg1 n = n `mod` 256
    reduceImm Reg4 n = (n + (1 `shiftL` 31)) `mod` (1 `shiftL` 32) - (1 `shiftL` 31)
    reduceImm Reg8 n = n
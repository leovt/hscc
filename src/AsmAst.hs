module AsmAst
  ( translateTACtoASM,
    emitProgram,
    Program (..),
    TopLevel (..),
    Instruction (..),
  )
where

import CTypes (ArithmeticType (..), CType (..), IntSize (..), IntegralType (..), Signed (..), intT, isIntegralType)
import Control.Monad.State
import Data.Bits (Bits (shiftL))
import qualified Data.Map
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64)
import Numeric (showHex)
import Parser (BinaryOperator (..), ConstValue (..), UnaryOperator (..))
import qualified Parser as P
import TAC (valueType)
import qualified TAC as T

{- HLINT ignore "Use newtype instead of data" -}
data Program
  = Program [TopLevel]
  deriving (Show)

data ConstantBits
  = Bits32 Word32
  | Bits64 Word64
  | Bits128 Word64
  deriving (Show, Eq, Ord)

data TopLevel
  = Function String Bool [Instruction]
  | StaticVariable AsmType String Bool ConstantBits
  | StaticConstant AsmType String ConstantBits
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
  | Cvttsd2si AsmType Operand Operand
  | Cvtsi2sd AsmType Operand Operand
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
  | DivDbl
  | And
  | Or
  | Xor
  | ShLeft
  | ShRight Signed
  deriving (Show)

data Condition = E | NE | G | GE | L | LE | A | AE | B | BE | PF
  deriving (Show)

data MemoryOperand
  = Stack Int String
  | Data String
  deriving (Show, Eq, Ord)

data Operand
  = Imm ConstValue
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
  | XMM0
  | XMM1
  | XMM2
  | XMM3
  | XMM4
  | XMM5
  | XMM6
  | XMM7
  | XMM14
  | XMM15
  deriving (Show, Eq, Ord)

data RegSize
  = Reg1
  | Reg4
  | Reg8
  | XMM
  deriving (Show, Eq, Ord)

data AsmType
  = Longword
  | Quadword
  | Double
  deriving (Show, Eq, Ord)

asmType :: CType -> AsmType
asmType (ArithmeticType (Integral (IType _ Int))) = Longword
asmType (ArithmeticType (Integral (IType _ Long))) = Quadword
asmType (ArithmeticType DoubleType) = Double
asmType _ = error "Unsupported CType for AsmType."

asmSign :: CType -> Signed
asmSign (ArithmeticType (Integral (IType s _))) = s
asmSign (ArithmeticType DoubleType) = Unsigned {- double comparison flags are same as unsigned -}
asmSign _ = Signed

asmValueType :: T.Value -> AsmType
asmValueType = asmType . valueType

asmValueSign :: T.Value -> Signed
asmValueSign = asmSign . valueType

type UniqueIdM a = State Int a

translateTACtoASM :: Int -> T.Program -> Program
translateTACtoASM n = fixImmediates . fixInstructions . replacePseudo . fixDoubleImmediates . translateProgram n
  where
    translateProgram :: Int -> T.Program -> Program
    translateProgram n p = evalState (translateProgram2 p) n

    uqName :: String -> UniqueIdM String
    uqName name = do
      n <- get
      modify (+ 1)
      return $ name ++ "." ++ show n

    translateProgram2 :: T.Program -> UniqueIdM Program
    translateProgram2 (T.Program functions) = do
      translated <- mapM translateTopLevel functions
      return $ Program translated

    translateTopLevel :: T.TopLevel -> UniqueIdM TopLevel
    translateTopLevel (T.Function name global params stmts) =
      do
        let (intRegArgs, dblRegArgs, stackArgs) = splitArgs params
            copyIntRegisterParameters = zipWith movarg intRegArgs intRegisters
            copyDoubleRegisterParameters = zipWith movarg dblRegArgs dblRegisters
            copyStackParameters = zipWith movstk stackArgs [16, 24 ..]

            movarg :: T.Value -> Reg -> Instruction
            movarg arg reg = TwoOp Mov (asmValueType arg) (Register reg) (translateValue arg)

            movstk :: T.Value -> Int -> Instruction
            movstk arg@(T.Variable _ _ name) offset = TwoOp Mov (asmValueType arg) (Memory $ Stack offset name) (translateValue arg)
            movstk _ _ = error "Parameters must be variables."
            cleanupStack = []
        body_instructions <- mapM translateInstruction stmts
        return $
          Function
            name
            global
            ( copyIntRegisterParameters
                ++ copyDoubleRegisterParameters
                ++ copyStackParameters
                ++ concat body_instructions
                ++ cleanupStack
            )
    translateTopLevel (T.StaticVariable t name global (IntValue init)) = pure $ StaticVariable (asmType t) name global init'
      where
        init' = case asmType t of
          Quadword -> Bits64 $ fromIntegral init
          Longword -> Bits32 $ fromIntegral init
          Double -> Bits64 (castDoubleToWord64 (fromIntegral init))
    translateTopLevel (T.StaticVariable t name global (DoubleValue init)) = pure $ StaticVariable (asmType t) name global init'
      where
        init' = case asmType t of
          Quadword -> Bits64 (truncate init)
          Longword -> Bits32 (truncate init)
          Double -> Bits64 (castDoubleToWord64 init)
    translateTopLevel (T.StaticVariable t name global (PackedDouble2Value init)) = pure $ StaticVariable (asmType t) name global init'
      where
        init' = case asmType t of
          Quadword -> Bits64 (truncate init)
          Longword -> Bits32 (truncate init)
          Double -> Bits64 (castDoubleToWord64 init)

    translateInstruction :: T.Instruction -> UniqueIdM [Instruction]
    translateInstruction (T.Return value) =
      pure
        [ TwoOp Mov (asmValueType value) (translateValue value) (returnRegister (asmValueType value)),
          Ret
        ]
    translateInstruction (T.Unary LogicNot src dst) = case valueType src of
      t@(ArithmeticType DoubleType) -> translateInstruction (T.Binary Equal src (T.Constant t (DoubleValue 0.0)) dst)
      t -> translateInstruction (T.Binary Equal src (T.Constant t (IntValue 0)) dst)
    translateInstruction (T.Unary op src dst)
      | op == Negate && valueType src == ArithmeticType DoubleType =
          pure
            [ TwoOp Mov (asmValueType dst) (translateValue src) (translateValue dst),
              TwoOp Xor Double (Imm (PackedDouble2Value (-0.0))) (translateValue dst)
            ]
      | otherwise =
          pure
            [ TwoOp Mov (asmValueType src) (translateValue src) (translateValue dst),
              OneOp (translateUnary op) (asmValueType src) (translateValue dst)
            ]
    translateInstruction (T.Binary Divide left right dst)
      | isIntegralType (valueType left) =
          pure
            [ TwoOp Mov (asmValueType left) (translateValue left) (Register AX),
              case asmValueSign left of
                Signed -> Cdq (asmValueType left)
                Unsigned -> TwoOp Mov (asmValueType left) (Imm (IntValue 0)) (Register DX),
              OneOp (Div (asmValueSign right)) (asmValueType right) (translateValue right),
              TwoOp Mov (asmValueType left) (Register AX) (translateValue dst)
            ]
      | otherwise =
          pure
            [ TwoOp Mov (asmValueType dst) (translateValue left) (translateValue dst),
              TwoOp DivDbl (asmValueType dst) (translateValue right) (translateValue dst)
            ]
    translateInstruction (T.Binary Remainder left right dst) =
      pure
        [ TwoOp Mov (asmValueType left) (translateValue left) (Register AX),
          case asmValueSign left of
            Signed -> Cdq (asmValueType left)
            Unsigned -> TwoOp Mov (asmValueType left) (Imm (IntValue 0)) (Register DX),
          OneOp (Div (asmValueSign right)) (asmValueType right) (translateValue right),
          TwoOp Mov (asmValueType left) (Register DX) (translateValue dst)
        ]
    translateInstruction (T.Binary op left right dst) =
      case translateBinary (asmValueSign left) op of
        Arithmetic instruction ->
          pure
            [ TwoOp Mov (asmValueType dst) (translateValue left) (translateValue dst),
              TwoOp instruction (asmValueType dst) (translateValue right) (translateValue dst)
            ]
        Relational condition -> case asmValueType left of
          Double -> do
            let dest = translateValue dst
            end_label <- uqName "skip_setcc"
            return
              [ TwoOp Cmp (asmValueType right) (translateValue right) (translateValue left),
                TwoOp Mov (asmType intT) (Imm (IntValue (if op == P.NotEqual then 1 else 0))) dest,
                JmpCC PF end_label,
                SetCC condition dest,
                Label end_label
              ]
          _ ->
            let dest = translateValue dst
             in pure
                  [ TwoOp Cmp (asmValueType right) (translateValue right) (translateValue left),
                    TwoOp Mov (asmType intT) (Imm (IntValue 0)) dest,
                    SetCC condition dest
                  ]
    translateInstruction (T.Copy src dst) =
      pure
        [ TwoOp Mov (asmValueType src) (translateValue src) (translateValue dst)
        ]
    translateInstruction (T.Jump label) = pure [Jmp label]
    translateInstruction (T.Label label) = pure [Label label]
    translateInstruction (T.JumpIfZero label value) =
      pure
        [ TwoOp Cmp (asmValueType value) (zero (asmValueType value)) (translateValue value),
          JmpCC E label
        ]
    translateInstruction (T.JumpIfNotZero label value) =
      pure
        [ TwoOp Cmp (asmValueType value) (zero (asmValueType value)) (translateValue value),
          JmpCC NE label
        ]
    translateInstruction (T.FunctionCall name args value) =
      pure $
        allocateStackSpace
          ++ passIntRegisterArguments
          ++ passDoubleRegisterArguments
          ++ passStackArguments
          ++ [Call name]
          ++ deallocateStackSpace
          ++ saveReturnValue
      where
        (intRegArgs, dblRegArgs, stackArgs) = splitArgs args
        stackPadding = 8 * mod (length stackArgs) 2
        cleanupSize = stackPadding + 8 * length stackArgs
        {- HLINT ignore "Use list comprehension" -}
        allocateStackSpace = if stackPadding == 0 then [] else [AllocateStack stackPadding]
        deallocateStackSpace = if cleanupSize == 0 then [] else [DeallocateStack cleanupSize]
        passIntRegisterArguments = zipWith movarg intRegArgs intRegisters
        passDoubleRegisterArguments = zipWith movarg dblRegArgs dblRegisters
        passStackArguments = concatMap movstk (reverse stackArgs)
        saveReturnValue = [TwoOp Mov (asmValueType value) (returnRegister (asmValueType value)) (translateValue value)]

        movarg :: T.Value -> Reg -> Instruction
        movarg val reg = TwoOp Mov (asmValueType val) (translateValue val) (Register reg)

        movstk :: T.Value -> [Instruction]
        movstk val = case asmValueType val of
          Longword -> case translateValue val of
            op@(Imm _) -> [Push op]
            op@(Register _) -> [Push op]
            op -> [TwoOp Mov (asmValueType val) op (Register AX), Push (Register AX)]
          Quadword -> [Push $ translateValue val]
          Double -> [Push $ translateValue val]
    translateInstruction (T.SignExtend src dst) = pure [MovSX (translateValue src) (translateValue dst)]
    translateInstruction (T.ZeroExtend src dst) = pure [MovZX (translateValue src) (translateValue dst)]
    translateInstruction (T.Truncate src dst) = pure [TwoOp Mov Longword (translateValue src) (translateValue dst)]
    translateInstruction (T.DoubleToInt src dst) = pure [Cvttsd2si (asmValueType dst) (translateValue src) (translateValue dst)]
    translateInstruction (T.DoubleToUInt src dst)
      | asmValueType dst == Quadword = do
          label_oob <- uqName "oob"
          label_end <- uqName "end"
          return
            [ TwoOp Cmp Double (Imm (DoubleValue longMaxPlus1)) (translateValue src),
              JmpCC AE label_oob,
              Cvttsd2si Quadword (translateValue src) (Register R11),
              TwoOp Mov (asmValueType dst) (Register R11) (translateValue dst),
              Jmp label_end,
              Label label_oob,
              TwoOp Mov Double (translateValue src) (Register XMM15),
              TwoOp Sub Double (Imm (DoubleValue longMaxPlus1)) (Register XMM15),
              Cvttsd2si Quadword (Register XMM15) (translateValue dst),
              TwoOp AsmAst.Add (asmValueType dst) (Imm (IntValue (longMax + 1))) (translateValue dst),
              Label label_end
            ]
      | otherwise =
          pure
            [ Cvttsd2si Quadword (translateValue src) (Register R11),
              TwoOp Mov (asmValueType dst) (Register R11) (translateValue dst)
            ]
    translateInstruction (T.IntToDouble src dst) = pure [Cvtsi2sd (asmValueType src) (translateValue src) (translateValue dst)]
    translateInstruction (T.UIntToDouble src dst)
      | asmValueType src == Longword =
          pure
            [ TwoOp Mov (asmValueType src) (translateValue src) (Register R10),
              Cvtsi2sd Quadword (Register R10) (translateValue dst)
            ]
      | otherwise = do
          label_oob <- uqName "oob"
          label_end <- uqName "end"
          return
            [ TwoOp Cmp Quadword (Imm (IntValue 0)) (translateValue src),
              JmpCC L label_oob,
              TwoOp Mov (asmValueType src) (translateValue src) (Register R10),
              Cvtsi2sd Quadword (Register R10) (translateValue dst),
              Jmp label_end,
              Label label_oob,
              TwoOp Mov Quadword (translateValue src) (Register R10),
              TwoOp Mov Quadword (Register R10) (Register R11),
              TwoOp And Quadword (Imm (IntValue 1)) (Register R11),
              TwoOp (ShRight Unsigned) Quadword (Imm (IntValue 1)) (Register R10),
              TwoOp Or Quadword (Register R11) (Register R10),
              Cvtsi2sd Quadword (Register R10) (Register XMM15),
              TwoOp AsmAst.Add Double (Register XMM15) (Register XMM15),
              TwoOp Mov Double (Register XMM15) (translateValue dst),
              Label label_end
            ]

    longMax :: Integer
    longMax = ((1 :: Integer) `shiftL` 63) - 1
    longMaxPlus1 :: Double
    longMaxPlus1 = 9223372036854775808.0 -- 2^63 as a double, the point at which unsigned values wrap around to negative when interpreted as signed
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

    returnRegister :: AsmType -> Operand
    returnRegister Double = Register XMM0
    returnRegister Longword = Register AX
    returnRegister Quadword = Register AX

    intRegisters :: [Reg]
    intRegisters = [DI, SI, DX, CX, R8, R9]
    dblRegisters :: [Reg]
    dblRegisters = [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7]
    splitArgs :: [T.Value] -> ([T.Value], [T.Value], [T.Value])
    splitArgs = foldl step ([], [], [])
      where
        step (r1, r2, r3) x
          | asmValueType x == Double && length r2 < length dblRegisters = (r1, r2 ++ [x], r3)
          | asmValueType x /= Double && length r1 < length intRegisters = (r1 ++ [x], r2, r3)
          | otherwise = (r1, r2, r3 ++ [x])

    zero :: AsmType -> Operand
    zero Longword = Imm (IntValue 0)
    zero Quadword = Imm (IntValue 0)
    zero Double = Imm (DoubleValue 0.0)

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
                Double -> 8
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
    replacePseudoIns (Cvtsi2sd t src dst) = do
      src' <- replacePseudoOp src
      dst' <- replacePseudoOp dst
      return (Cvtsi2sd t src' dst')
    replacePseudoIns (Cvttsd2si t src dst) = do
      src' <- replacePseudoOp src
      dst' <- replacePseudoOp dst
      return (Cvttsd2si t src' dst')
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

    scratchDstReg :: AsmType -> Operand
    scratchDstReg Longword = Register R11
    scratchDstReg Quadword = Register R11
    scratchDstReg Double = Register XMM15

    scratchSrcReg :: AsmType -> Operand
    scratchSrcReg Longword = Register R10
    scratchSrcReg Quadword = Register R10
    scratchSrcReg Double = Register XMM14

    fixInstr :: Instruction -> [Instruction]
    fixInstr (TwoOp Mul t src dst@(Memory _)) =
      [ TwoOp Mov t dst (scratchDstReg t),
        TwoOp Mul t src (scratchDstReg t),
        TwoOp Mov t (scratchDstReg t) dst
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
      [ TwoOp Mov t (Imm n) (scratchDstReg t),
        TwoOp Cmp t src (scratchDstReg t)
      ]
    fixInstr (TwoOp Cmp t@Double src@(Memory _) dst@(Memory _)) =
      [ TwoOp Mov t dst (scratchDstReg t),
        TwoOp Cmp t src (scratchDstReg t)
      ]
    fixInstr (TwoOp Mov t@Double src@(Memory _) dst@(Memory _)) =
      [ TwoOp Mov t src (scratchDstReg t),
        TwoOp Mov t (scratchDstReg t) dst
      ]
    fixInstr (TwoOp op t@Double src@(Memory _) dst@(Memory _)) =
      [ TwoOp Mov t dst (scratchDstReg t),
        TwoOp op t src (scratchDstReg t),
        TwoOp Mov t (scratchDstReg t) dst
      ]
    fixInstr (TwoOp op t src@(Memory _) dst@(Memory _)) =
      [ TwoOp Mov t src (scratchSrcReg t),
        TwoOp op t (scratchSrcReg t) dst
      ]
    fixInstr (OneOp op@(Div _) t (Imm n)) =
      [ TwoOp Mov t (Imm n) (scratchSrcReg t),
        OneOp op t (scratchSrcReg t)
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
    fixInstr (Cvttsd2si t src dst@(Memory _)) =
      [ Cvttsd2si t src (scratchDstReg t),
        TwoOp Mov t (scratchDstReg t) dst
      ]
    fixInstr (Cvtsi2sd t src@(Imm _) dst@(Memory _)) =
      [ TwoOp Mov t src (scratchSrcReg t),
        Cvtsi2sd t (scratchSrcReg t) (scratchDstReg Double),
        TwoOp Mov Double (scratchDstReg Double) dst
      ]
    fixInstr (Cvtsi2sd t src dst@(Memory _)) =
      [ Cvtsi2sd t src (scratchDstReg Double),
        TwoOp Mov Double (scratchDstReg Double) dst
      ]
    fixInstr (Cvtsi2sd t src@(Imm _) dst) =
      [ TwoOp Mov t src (scratchSrcReg t),
        Cvtsi2sd t (scratchSrcReg t) dst
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
    fixInstr ins@(TwoOp op Quadword src@(Imm (IntValue n)) dst)
      | fitsImm32Signed n = [ins]
      | otherwise =
          [ TwoOp Mov Quadword src (Register R10),
            TwoOp op Quadword (Register R10) dst
          ]
    fixInstr ins@(Push src@(Imm (IntValue n)))
      | fitsImm32Signed n = [ins]
      | otherwise =
          [ TwoOp Mov Quadword src (Register R10),
            Push (Register R10)
          ]
    fixInstr ins = [ins]

    fitsImm32Signed :: Integer -> Bool
    fitsImm32Signed n =
      n >= -(1 `shiftL` 31) && n <= (1 `shiftL` 31) - 1

data FixDoubleImmState = FixDoubleImmState
  { constLabels :: Data.Map.Map ConstantBits String
  }

type FixDbImmM a = State FixDoubleImmState a -- the translation monad encapsulating the translation state

fixDoubleImmediates :: Program -> Program
fixDoubleImmediates program = evalState (fixImmProg program) (FixDoubleImmState {constLabels = Data.Map.empty})
  where
    fixImmProg :: Program -> FixDbImmM Program
    fixImmProg (Program tops) = do
      tops' <- mapM fixImmediatesTop tops
      state <- get
      return $ Program (tops' ++ createStaticConst (constLabels state))

    createStaticConst :: Data.Map.Map ConstantBits String -> [TopLevel]
    createStaticConst constLabels = map fromConstantBits (Data.Map.toList constLabels)

    fromConstantBits :: (ConstantBits, String) -> TopLevel
    fromConstantBits (cb, name) = StaticConstant Double name cb

    fixImmediatesTop :: TopLevel -> FixDbImmM TopLevel
    fixImmediatesTop (Function name global instructions) = do
      instructions' <- mapM fixInstr instructions
      return $ Function name global (concat instructions')
    fixImmediatesTop other = pure other

    fixInstr :: Instruction -> FixDbImmM [Instruction]
    fixInstr (Cvttsd2si t src dst) = do
      src' <- fixOp src
      dst' <- fixOp dst
      return [Cvttsd2si t src' dst']
    fixInstr (TwoOp op t src dst) = do
      src' <- fixOp src
      dst' <- fixOp dst
      return [TwoOp op t src' dst']
    fixInstr (Push src) = do
      src' <- fixOp src
      return [Push src']
    fixInstr ins = pure [ins]

    fixOp :: Operand -> FixDbImmM Operand
    fixOp (Imm (DoubleValue d)) = do
      let bits = castDoubleToWord64 d
      state <- get
      case Data.Map.lookup (Bits64 bits) (constLabels state) of
        Just label -> return $ Memory (Data label)
        Nothing -> do
          let label = "const_double_" ++ showHex bits ""
          put $ state {constLabels = Data.Map.insert (Bits64 bits) label (constLabels state)}
          return $ Memory (Data label)
    fixOp (Imm (PackedDouble2Value d)) = do
      let bits = castDoubleToWord64 d
      state <- get
      case Data.Map.lookup (Bits128 bits) (constLabels state) of
        Just label -> return $ Memory (Data label)
        Nothing -> do
          let label = "const_double_" ++ showHex bits ""
          put $ state {constLabels = Data.Map.insert (Bits128 bits) label (constLabels state)}
          return $ Memory (Data label)
    fixOp op = pure op

emitProgram :: Program -> [String]
emitProgram (Program fun) = concatMap emitTopLevel fun ++ [".section .note.GNU-stack,\"\",@progbits"]
  where
    emitTopLevel :: TopLevel -> [String]
    emitTopLevel (Function name global instructions) =
      asmglobal global name ++ [name ++ ":", "    pushq %rbp", "    movq %rsp, %rbp"] ++ map emitInstruction instructions
    emitTopLevel (StaticVariable t name global init) =
      asmglobal global name ++ [".data", alignmentOf init, name ++ ":", emitStaticData t init, ".text"]
    emitTopLevel (StaticConstant t name init) =
      [".section .rodata", alignmentOf init, name ++ ":", emitStaticData t init, ".text"]

    asmglobal :: Bool -> String -> [String]
    asmglobal global name = if global then [".globl " ++ name] else []

    alignmentOf :: ConstantBits -> String
    alignmentOf (Bits32 _) = ".align 4"
    alignmentOf (Bits64 _) = ".align 8"
    alignmentOf (Bits128 _) = ".align 16"

    emitStaticData :: AsmType -> ConstantBits -> String
    emitStaticData Longword (Bits32 x) = "    .long 0x" ++ showHex x ""
    emitStaticData Quadword (Bits64 x) = "    .quad 0x" ++ showHex x ""
    emitStaticData Double (Bits64 x) = "    .quad 0x" ++ showHex x ""
    emitStaticData Double (Bits128 x) = "    .quad 0x" ++ showHex x "" ++ "\n    .quad 0"
    emitStaticData _ _ = error "Type mismatch between static variable and its initializer."

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
    emitInstruction (Cdq _) = error "Internal Error: Cdq for non-integer type"
    emitInstruction (Jmp label) = "    jmp " ++ label
    emitInstruction (Label label) = label ++ ":"
    emitInstruction (JmpCC condition label) = "    j" ++ cond condition ++ " " ++ label
    emitInstruction (SetCC condition dst) = "    set" ++ cond condition ++ " " ++ emitOperand Reg4 dst
    emitInstruction (Push src) = "    pushq " ++ emitOperand Reg8 src
    emitInstruction (Call name) = "    call " ++ name
    emitInstruction (MovSX src dst) = "    movslq " ++ emitOperand Reg4 src ++ ", " ++ emitOperand Reg8 dst
    emitInstruction (MovZX _ _) = error "emitInstruction: MovZX not implemented, should be removed in fixInstructions"
    emitInstruction (Cvttsd2si t src dst) = "    cvttsd2si" ++ typeSuffix t ++ " " ++ emitOperand XMM src ++ ", " ++ emitOperand (regSize t) dst
    emitInstruction (Cvtsi2sd t src dst) = "    cvtsi2sd" ++ typeSuffix t ++ " " ++ emitOperand (regSize t) src ++ ", " ++ emitOperand XMM dst

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
    twoOp Mul Double = "mul" ++ typeSuffix Double
    twoOp Mul t = "imul" ++ typeSuffix t
    twoOp DivDbl t = "div" ++ typeSuffix t
    twoOp And t = "and" ++ typeSuffix t
    twoOp Or t = "or" ++ typeSuffix t
    twoOp Xor Double = "xorpd"
    twoOp Xor t = "xor" ++ typeSuffix t
    twoOp ShLeft t = "sal" ++ typeSuffix t
    twoOp (ShRight Signed) t = "sar" ++ typeSuffix t
    twoOp (ShRight Unsigned) t = "shr" ++ typeSuffix t
    twoOp Cmp Double = "ucomisd"
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
    cond PF = "p"

    typeSuffix :: AsmType -> String
    typeSuffix Longword = "l"
    typeSuffix Quadword = "q"
    typeSuffix Double = "sd"

    regSize :: AsmType -> RegSize
    regSize Longword = Reg4
    regSize Quadword = Reg8
    regSize Double = XMM

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
    emitOperand XMM (Register XMM0) = "%xmm0"
    emitOperand XMM (Register XMM1) = "%xmm1"
    emitOperand XMM (Register XMM2) = "%xmm2"
    emitOperand XMM (Register XMM3) = "%xmm3"
    emitOperand XMM (Register XMM4) = "%xmm4"
    emitOperand XMM (Register XMM5) = "%xmm5"
    emitOperand XMM (Register XMM6) = "%xmm6"
    emitOperand XMM (Register XMM7) = "%xmm7"
    emitOperand XMM (Register XMM14) = "%xmm14"
    emitOperand XMM (Register XMM15) = "%xmm15"
    emitOperand sz reg = error $ "Internal Error: Invalid Register-Size combination in emitOperand: " ++ show sz ++ " and " ++ show reg

    reduceImm :: RegSize -> ConstValue -> Integer
    reduceImm Reg1 (IntValue n) = n `mod` 256
    reduceImm Reg4 (IntValue n) = (n + (1 `shiftL` 31)) `mod` (1 `shiftL` 32) - (1 `shiftL` 31)
    reduceImm Reg8 (IntValue n) = n
    reduceImm _ dv@(DoubleValue _) = error $ "Internal Error: double valued immediate " ++ show dv ++ " can not be emitted."
    reduceImm _ dv@(PackedDouble2Value _) = error $ "Internal Error: packed double valued immediate " ++ show dv ++ " can not be emitted."
    reduceImm XMM val = error $ "Internal Error: use XMM for integers: " ++ show val
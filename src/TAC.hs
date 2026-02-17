module TAC
  ( translate,
    Program (..),
    TopLevel (..),
    Instruction (..),
    Value (..),
    valueType,
  )
where

import CTypes (ArithmeticType (..), CType (..), IntSize (..), IntegralType (..), Signed (..))
import Control.Monad.State
import Data.Map (toList)
import qualified Data.Map
import Data.Maybe (catMaybes, fromJust)
import Parser
  ( BinaryOperator,
    ConstValue (..),
    UnaryOperator,
  )
import qualified Parser as P
import Validate
  ( Initializer (..),
    SwitchLabels (..),
    SymbolAttributes (..),
    SymbolInfo (..),
    SymbolTable (..),
    typeOf,
    zero,
  )

{- HLINT ignore "Use newtype instead of data" -}
data Program
  = Program [TopLevel]
  deriving (Show)

data TopLevel
  = Function String Bool [Value] [Instruction]
  | StaticVariable CType String Bool ConstValue
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
  | FunctionCall String [Value] Value
  | SignExtend Value Value
  | ZeroExtend Value Value
  | Truncate Value Value
  | DoubleToInt Value Value
  | IntToDouble Value Value
  | DoubleToUInt Value Value
  | UIntToDouble Value Value
  deriving (Show)

data Value
  = Constant CType ConstValue
  | Variable CType Bool String {- static duration, name -}
  deriving (Show)

valueType :: Value -> CType
valueType (Constant t _) = t
valueType (Variable t _ _) = t

data TransState = TransState
  { nextID :: Int,
    breakLabel :: Maybe String,
    continueLabel :: Maybe String,
    switchLabels :: Maybe (Data.Map.Map SwitchLabels String)
  }

type TransM a = State TransState a -- the translation monad encapsulating the translation state

newId :: String -> TransM String
newId prefix = do
  state <- get
  let n = nextID state
  put state {nextID = n + 1}
  return $ prefix ++ "." ++ show n

translate :: P.TypedProgram -> SymbolTable -> Int -> (Program, Int)
translate program (SymbolTable symtab) nextID' = (prog, nextID state)
  where
    (prog, state) = runState (translateProgram program) initState
    initState =
      TransState
        { nextID = nextID',
          breakLabel = Nothing,
          continueLabel = Nothing,
          switchLabels = Nothing
        }

    translateProgram :: P.TypedProgram -> TransM Program
    translateProgram (P.Program decls) = do
      let staticVariables = map translateSymbol (toList symtab)
      functions <- mapM translateDeclaration decls
      return (Program (catMaybes (staticVariables ++ functions)))

    translateSymbol :: (String, SymbolInfo) -> Maybe TopLevel
    translateSymbol (name, SymbolInfo varT (StaticVariableAttr (Initial init) global)) =
      Just $ StaticVariable varT name global init
    translateSymbol (name, SymbolInfo varT (StaticVariableAttr Tentative global)) =
      Just $ StaticVariable varT name global (zero varT)
    translateSymbol _ = Nothing

    translateDeclaration :: P.Declaration CType -> TransM (Maybe TopLevel)
    translateDeclaration (P.FunDecl fun) = translateFunction fun
    translateDeclaration _ = return Nothing

    translateFunction :: P.FunctionDeclaration CType -> TransM (Maybe TopLevel)
    translateFunction (P.FunctionDeclaration retT name params (Just body) _ _) = do
      instructions <- translateBlock body
      let global = case Data.Map.lookup name symtab of
            Nothing -> error $ "symbol not found " ++ name
            Just (SymbolInfo _type (FunctionAttr _ g)) -> g
            _ -> error $ "symbol a function symbol " ++ name
      return $ Just (Function name global (map (\(paramT, name) -> Variable paramT False (fromJust name)) params) (instructions ++ [Return (Constant retT (zero retT))]))
    translateFunction (P.FunctionDeclaration _ _ _ Nothing _ _) = return Nothing

    translateBlock :: P.Block CType -> TransM [Instruction]
    translateBlock (P.Block items) = concat <$> mapM translateBlockItem items

    translateBlockItem :: P.BlockItem CType -> TransM [Instruction]
    translateBlockItem (P.Stmt s) = translateStatement s
    translateBlockItem (P.Decl (P.VarDecl (P.VariableDeclaration _ _ _ P.StorageStatic _))) = return []
    translateBlockItem (P.Decl (P.VarDecl (P.VariableDeclaration retT name (Just expr) _ _))) = do
      (instr, _value) <- translateExpression (P.Binary retT P.Assignment (P.Variable retT name) expr)
      return instr
    translateBlockItem (P.Decl _) = return []

    translateStatement :: P.Statement CType -> TransM [Instruction]
    translateStatement (P.ReturnStatement expression) = do
      (instructions, value) <- translateExpression expression
      return (instructions ++ [Return value])
    translateStatement (P.ExpressionStatement expression) = do
      (instructions, _value) <- translateExpression expression
      return instructions
    translateStatement P.NullStatement = return []
    translateStatement (P.IfStatement cond thenStmt maybeElseStmt) = do
      (cond_instructions, cond_value) <- translateExpression cond
      then_instructions <- translateStatement thenStmt
      end_label <- newId "if.end"
      else_label <- newId "if.else"

      let jump_label = case maybeElseStmt of
            Just _ -> else_label
            Nothing -> end_label

      else_block <- case maybeElseStmt of
        Just elseStmt -> do
          else_instructions <- translateStatement elseStmt
          return $ [Jump end_label, Label else_label] ++ else_instructions
        Nothing -> return []

      return
        ( cond_instructions
            ++ [JumpIfZero jump_label cond_value]
            ++ then_instructions
            ++ else_block
            ++ [Label end_label]
        )
    translateStatement (P.GotoStatement labelName) = do
      return [Jump labelName]
    translateStatement (P.CompoundStatement block) = translateBlock block
    translateStatement (P.DoWhileStatement cond stmt) = do
      begin_label <- newId "do.begin"
      continue_label <- newId "do.continue"
      break_label <- newId "do.break"
      state_before <- get
      put state_before {continueLabel = Just continue_label, breakLabel = Just break_label}
      stmt_instructions <- translateStatement stmt
      state_after <- get
      put state_after {continueLabel = continueLabel state_before, breakLabel = breakLabel state_before}
      (cond_instructions, cond_value) <- translateExpression cond
      return
        ( [Label begin_label]
            ++ stmt_instructions
            ++ [Label continue_label]
            ++ cond_instructions
            ++ [JumpIfNotZero begin_label cond_value, Label break_label]
        )
    translateStatement (P.WhileStatement cond stmt) = do
      (cond_instructions, cond_value) <- translateExpression cond
      continue_label <- newId "while.continue"
      break_label <- newId "while.break"
      state_before <- get
      put state_before {continueLabel = Just continue_label, breakLabel = Just break_label}
      stmt_instructions <- translateStatement stmt
      state_after <- get
      put state_after {continueLabel = continueLabel state_before, breakLabel = breakLabel state_before}
      return
        ( [Label continue_label]
            ++ cond_instructions
            ++ [JumpIfZero break_label cond_value]
            ++ stmt_instructions
            ++ [Jump continue_label, Label break_label]
        )
    translateStatement (P.ForStatement maybeInit maybeCond maybeInc stmt) = do
      begin_label <- newId "for.begin"
      continue_label <- newId "for.continue"
      break_label <- newId "for.break"
      init_instructions <- case maybeInit of
        Nothing -> return []
        Just (P.ForInitExpr expr) -> do
          (instr, _value) <- translateExpression expr
          return instr
        Just (P.ForInitDecl (P.VarDecl (P.VariableDeclaration varT name (Just expr) _ _))) -> do
          (instr, _value) <- translateExpression (P.Binary varT P.Assignment (P.Variable varT name) expr)
          return instr
        Just (P.ForInitDecl _) -> return []
      cond_instructions <- case maybeCond of
        Nothing -> return []
        Just expr -> do
          (instr, value) <- translateExpression expr
          return (instr ++ [JumpIfZero break_label value])
      inc_instructions <- case maybeInc of
        Nothing -> return []
        Just expr -> do
          (instr, _value) <- translateExpression expr
          return instr
      state_before <- get
      put state_before {continueLabel = Just continue_label, breakLabel = Just break_label}
      stmt_instructions <- translateStatement stmt
      state_after <- get
      put state_after {continueLabel = continueLabel state_before, breakLabel = breakLabel state_before}
      return
        ( init_instructions
            ++ [Label begin_label]
            ++ cond_instructions
            ++ stmt_instructions
            ++ [Label continue_label]
            ++ inc_instructions
            ++ [Jump begin_label, Label break_label]
        )
    translateStatement P.BreakStatement = do
      state <- get
      case breakLabel state of
        Just label -> return [Jump label]
        Nothing -> error "Break statement not within a loop."
    translateStatement P.ContinueStatement = do
      state <- get
      case continueLabel state of
        Just label -> return [Jump label]
        Nothing -> error "Continue statement not within a loop."
    translateStatement (P.SwitchStatement expr stmt) = do
      (cond_instructions, cond_value) <- translateExpression expr
      break_label <- newId "switch.break"
      state_before <- get
      put state_before {breakLabel = Just break_label, switchLabels = Just Data.Map.empty}
      stmt_instructions <- translateStatement stmt
      state_after <- get
      put state_after {breakLabel = breakLabel state_before, switchLabels = switchLabels state_before}
      {-
      for each (caseValue, labelName) in switchLabels state_after
        add instruction: JumpIfEqual labelName cond_value caseValue
      -}
      let switchLabelsMap :: Data.Map.Map SwitchLabels String
          switchLabelsMap = fromJust (switchLabels state_after)
          case_jump :: (SwitchLabels, String) -> TransM [Instruction]
          case_jump (Default, _) = return []
          case_jump (Case n, jump_target) = do
            varid <- newId "tmp"
            let destination = Variable (typeOf expr) False varid
            return
              [ Binary P.Equal cond_value (Constant (typeOf expr) (IntValue n)) destination,
                JumpIfNotZero jump_target destination
              ]
      case_jumps_list <- mapM case_jump (Data.Map.toList switchLabelsMap)
      let case_jumps = concat case_jumps_list
          default_jump = case Data.Map.lookup Default switchLabelsMap of
            Just label -> [Jump label]
            Nothing -> [Jump break_label]
      return
        ( cond_instructions
            ++ case_jumps
            ++ default_jump
            ++ stmt_instructions
            ++ [Label break_label]
        )
    translateStatement (P.LabelledStatement (P.Label labelName) stmt) = do
      stmt_instructions <- translateStatement stmt
      return (Label labelName : stmt_instructions)
    translateStatement (P.LabelledStatement (P.CaseLabel value) stmt) = do
      label <- newId $ "switch.case_" ++ if value < 0 then "neg_" ++ show (-value) else show value
      stmt_instructions <- translateStatement stmt
      state <- get
      let switchLabelsMap = fromJust (switchLabels state)
          switchLabelsMap' = Data.Map.insert (Case value) label switchLabelsMap
      put state {switchLabels = Just switchLabelsMap'}
      return (Label label : stmt_instructions)
    translateStatement (P.LabelledStatement P.DefaultLabel stmt) = do
      label <- newId "switch.default"
      stmt_instructions <- translateStatement stmt
      state <- get
      let switchLabelsMap = fromJust (switchLabels state)
          switchLabelsMap' = Data.Map.insert Default label switchLabelsMap
      put state {switchLabels = Just switchLabelsMap'}
      return (Label label : stmt_instructions)

    translateExpression :: P.Expression CType -> TransM ([Instruction], Value)
    translateExpression (P.Constant t c) = do
      return ([], Constant t c)
    translateExpression (P.Unary t P.PreIncrement var@(P.Variable _ _)) = do
      varid <- newId "tmp"
      let destination = Variable t False varid
      (_, var') <- translateExpression var
      return
        ( [ Binary P.Add var' (Constant t (one t)) destination,
            Copy destination var'
          ],
          destination
        )
    translateExpression (P.Unary t P.PreDecrement var@(P.Variable _ _)) = do
      varid <- newId "tmp"
      let destination = Variable t False varid
      (_, var') <- translateExpression var
      return
        ( [ Binary P.Subtract var' (Constant t (one t)) destination,
            Copy destination var'
          ],
          destination
        )
    translateExpression (P.Unary t P.PostIncrement var@(P.Variable _ _)) = do
      varid <- newId "tmp"
      let destination = Variable t False varid
      varid <- newId "tmp"
      let newvalue = Variable t False varid
      (_, var') <- translateExpression var
      return
        ( [ Copy var' destination,
            Binary P.Add destination (Constant t (one t)) newvalue,
            Copy newvalue var'
          ],
          destination
        )
    translateExpression (P.Unary t P.PostDecrement var@(P.Variable _ _)) = do
      varid <- newId "tmp"
      let destination = Variable t False varid
      varid <- newId "tmp"
      let newvalue = Variable t False varid
      (_, var') <- translateExpression var
      return
        ( [ Copy var' destination,
            Binary P.Subtract destination (Constant t (one t)) newvalue,
            Copy newvalue var'
          ],
          destination
        )
    translateExpression (P.Unary _ P.PostDecrement _) = error "PostDecrement on non-variable."
    translateExpression (P.Unary _ P.PostIncrement _) = error "PostIncrement on non-variable."
    translateExpression (P.Unary _ P.PreDecrement _) = error "PreDecrement on non-variable."
    translateExpression (P.Unary _ P.PreIncrement _) = error "PreIncrement on non-variable."
    translateExpression (P.Unary t op expression) = do
      (instructions, value) <- translateExpression expression
      varid <- newId "tmp"
      let destination = Variable t False varid
      return (instructions ++ [Unary op value destination], destination)
    translateExpression (P.Binary t P.LogicAnd left right) = do
      (l_instructions, left') <- translateExpression left
      (r_instructions, right') <- translateExpression right
      varid <- newId "tmp"
      false_label <- newId "false.label"
      end_label <- newId "end.label"
      let destination = Variable t False varid
      let instructions =
            l_instructions
              ++ [JumpIfZero false_label left']
              ++ r_instructions
              ++ [ JumpIfZero false_label right',
                   Copy (Constant t (IntValue 1)) destination,
                   Jump end_label,
                   Label false_label,
                   Copy (Constant t (IntValue 0)) destination,
                   Label end_label
                 ]
      return (instructions, destination)
    translateExpression (P.Binary t P.LogicOr left right) = do
      (l_instructions, left') <- translateExpression left
      (r_instructions, right') <- translateExpression right
      varid <- newId "tmp"
      true_label <- newId "true.label"
      end_label <- newId "end.label"
      let destination = Variable t False varid
      let instructions =
            l_instructions
              ++ [JumpIfNotZero true_label left']
              ++ r_instructions
              ++ [ JumpIfNotZero true_label right',
                   Copy (Constant t (IntValue 0)) destination,
                   Jump end_label,
                   Label true_label,
                   Copy (Constant t (IntValue 1)) destination,
                   Label end_label
                 ]
      return (instructions, destination)
    translateExpression (P.Binary _ P.Assignment left@(P.Variable _ _) right) = do
      (l_instructions, left') <- translateExpression left
      (r_instructions, right') <- translateExpression right
      return (l_instructions ++ r_instructions ++ [Copy right' left'], right')
    translateExpression (P.Binary t (P.CompoundAssignment op) left@(P.Variable _ _) right) = do
      (l_instructions, left') <- translateExpression left
      (r_instructions, right') <- translateExpression right
      varid <- newId "tmp"
      let destination = Variable t False varid
      return
        ( l_instructions
            ++ r_instructions
            ++ [Binary op left' right' destination, Copy destination left'],
          destination
        )
    translateExpression expr@(P.Binary _ P.Assignment _ _) = error $ "assign to non-variable: " ++ show expr
    translateExpression expr@(P.Binary _ (P.CompoundAssignment _) _ _) = error $ "assign to non-variable: " ++ show expr
    translateExpression (P.Binary t op left right) = do
      (l_instructions, left') <- translateExpression left
      (r_instructions, right') <- translateExpression right
      varid <- newId "tmp"
      let destination = Variable t False varid
      return (l_instructions ++ r_instructions ++ [Binary op left' right' destination], destination)
    translateExpression (P.Variable t name) = do
      let symbol = case Data.Map.lookup name symtab of
            Nothing -> error $ "symbol not found: " ++ name
            Just info -> info
      case symbol of
        SymbolInfo _ (StaticVariableAttr _ _) -> return ([], Variable t True name)
        SymbolInfo _ (FunctionAttr _ _) -> error "variable expected, function found."
        SymbolInfo _ LocalVariableAttr -> return ([], Variable t False name)
    translateExpression (P.Conditional t condExpr thenExpr elseExpr) = do
      (cond_instructions, cond_value) <- translateExpression condExpr
      else_label <- newId "cond.else"
      end_label <- newId "cond.end"
      (then_instructions, then_value) <- translateExpression thenExpr
      (else_instructions, else_value) <- translateExpression elseExpr
      varid <- newId "tmp.cond"
      let destination = Variable t False varid
      let instructions =
            cond_instructions
              ++ [JumpIfZero else_label cond_value]
              ++ then_instructions
              ++ [Copy then_value destination, Jump end_label, Label else_label]
              ++ else_instructions
              ++ [Copy else_value destination, Label end_label]
      return (instructions, destination)
    translateExpression (P.FunctionCall t name args) = do
      pairs <- mapM translateExpression args
      let (instructions, args') = unzip pairs
      varid <- newId "tmp.cond"
      let destination = Variable t False varid
      return (concat instructions ++ [FunctionCall name args' destination], destination)
    translateExpression (P.Cast t expr)
      | typeOf expr == t = translateExpression expr
      | otherwise = do
          (instructions, value) <- translateExpression expr
          varid <- newId "tmp.cast"
          let destination = Variable t False varid
          let cast_instruction = case (typeOf expr, t) of
                (ArithmeticType (Integral (IType s a)), ArithmeticType (Integral (IType _ b)))
                  | a == b -> Copy value destination
                  | a == Int && s == Signed -> SignExtend value destination
                  | a == Int && s == Unsigned -> ZeroExtend value destination
                  | otherwise -> Truncate value destination
                (ArithmeticType (Integral (IType Signed _)), ArithmeticType DoubleType) ->
                  IntToDouble value destination
                (ArithmeticType (Integral (IType Unsigned _)), ArithmeticType DoubleType) ->
                  UIntToDouble value destination
                (ArithmeticType DoubleType, ArithmeticType (Integral (IType Signed _))) ->
                  DoubleToInt value destination
                (ArithmeticType DoubleType, ArithmeticType (Integral (IType Unsigned _))) ->
                  DoubleToUInt value destination
                _ -> error "Unsupported cast."
          return (instructions ++ [cast_instruction], destination)
    one :: CType -> ConstValue
    one (ArithmeticType (Integral _)) = IntValue 1
    one (ArithmeticType DoubleType) = DoubleValue 1.0
    one _ = error "Implementation Error: one for non-arithmetic type"
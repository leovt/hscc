module TAC
  ( translate,
    Program (..),
    Function (..),
    Instruction (..),
    Value (..),
  )
where

import Control.Monad.State
import qualified Data.Map
import Data.Maybe (fromJust)
import Parser
  ( BinaryOperator,
    UnaryOperator,
  )
import qualified Parser as P
import Validate (SwitchLabels (..))

{- HLINT ignore "Use newtype instead of data" -}
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

translate :: P.Program -> Int -> Program
translate program nextID' = evalState (translateProgram program) initState
  where
    initState =
      TransState
        { nextID = nextID',
          breakLabel = Nothing,
          continueLabel = Nothing,
          switchLabels = Nothing
        }

    translateProgram :: P.Program -> TransM Program
    translateProgram (P.Program fun) = do
      fun' <- translateFunction fun
      return (Program fun')

    translateFunction :: P.Function -> TransM Function
    translateFunction (P.Function name body) = do
      instructions <- translateBlock body
      return (Function name (instructions ++ [Return (Constant 0)]))

    translateBlock :: P.Block -> TransM [Instruction]
    translateBlock (P.Block items) = concat <$> mapM translateBlockItem items

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
        Just (P.ForInitDecl (P.VariableDeclaration name (Just expr))) -> do
          (instr, _value) <- translateExpression (P.Binary P.Assignment (P.Variable name) expr)
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
            let destination = Variable varid
            return
              [ Binary P.Equal cond_value (Constant n) destination,
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
      label <- newId $ "switch.case" ++ show value
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

    translateExpression :: P.Expression -> TransM ([Instruction], Value)
    translateExpression (P.Constant c) = do
      return ([], Constant c)
    translateExpression (P.Unary P.PreIncrement (P.Variable name)) = do
      varid <- newId "tmp"
      let destination = Variable varid
      return
        ( [ Binary P.Add (Variable name) (Constant 1) destination,
            Copy destination (Variable name)
          ],
          destination
        )
    translateExpression (P.Unary P.PreDecrement (P.Variable name)) = do
      varid <- newId "tmp"
      let destination = Variable varid
      return
        ( [ Binary P.Subtract (Variable name) (Constant 1) destination,
            Copy destination (Variable name)
          ],
          destination
        )
    translateExpression (P.Unary P.PostIncrement (P.Variable name)) = do
      varid <- newId "tmp"
      let destination = Variable varid
      varid <- newId "tmp"
      let newvalue = Variable varid
      return
        ( [ Copy (Variable name) destination,
            Binary P.Add destination (Constant 1) newvalue,
            Copy newvalue (Variable name)
          ],
          destination
        )
    translateExpression (P.Unary P.PostDecrement (P.Variable name)) = do
      varid <- newId "tmp"
      let destination = Variable varid
      varid <- newId "tmp"
      let newvalue = Variable varid
      return
        ( [ Copy (Variable name) destination,
            Binary P.Subtract destination (Constant 1) newvalue,
            Copy newvalue (Variable name)
          ],
          destination
        )
    translateExpression (P.Unary P.PostDecrement _) = error "PostDecrement on non-variable."
    translateExpression (P.Unary P.PostIncrement _) = error "PostIncrement on non-variable."
    translateExpression (P.Unary P.PreDecrement _) = error "PreDecrement on non-variable."
    translateExpression (P.Unary P.PreIncrement _) = error "PreIncrement on non-variable."
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
            l_instructions
              ++ [JumpIfZero false_label left']
              ++ r_instructions
              ++ [ JumpIfZero false_label right',
                   Copy (Constant 1) destination,
                   Jump end_label,
                   Label false_label,
                   Copy (Constant 0) destination,
                   Label end_label
                 ]
      return (instructions, destination)
    translateExpression (P.Binary P.LogicOr left right) = do
      (l_instructions, left') <- translateExpression left
      (r_instructions, right') <- translateExpression right
      varid <- newId "tmp"
      true_label <- newId "true.label"
      end_label <- newId "end.label"
      let destination = Variable varid
      let instructions =
            l_instructions
              ++ [JumpIfNotZero true_label left']
              ++ r_instructions
              ++ [ JumpIfNotZero true_label right',
                   Copy (Constant 0) destination,
                   Jump end_label,
                   Label true_label,
                   Copy (Constant 1) destination,
                   Label end_label
                 ]
      return (instructions, destination)
    translateExpression (P.Binary P.Assignment (P.Variable left) right) = do
      (r_instructions, right') <- translateExpression right
      return (r_instructions ++ [Copy right' (Variable left)], right')
    translateExpression (P.Binary (P.CompoundAssignment op) (P.Variable left) right) = do
      (l_instructions, left') <- translateExpression (P.Variable left)
      (r_instructions, right') <- translateExpression right
      varid <- newId "tmp"
      let destination = Variable varid
      return
        ( l_instructions
            ++ r_instructions
            ++ [Binary op left' right' destination, Copy destination (Variable left)],
          destination
        )
    translateExpression (P.Binary P.Assignment _ _) = error "assign to non-variable."
    translateExpression (P.Binary (P.CompoundAssignment _) _ _) = error "assign to non-variable."
    translateExpression (P.Binary op left right) = do
      (l_instructions, left') <- translateExpression left
      (r_instructions, right') <- translateExpression right
      varid <- newId "tmp"
      let destination = Variable varid
      return (l_instructions ++ r_instructions ++ [Binary op left' right' destination], destination)
    translateExpression (P.Variable name) = do
      return ([], Variable name)
    translateExpression (P.Conditional condExpr thenExpr elseExpr) = do
      (cond_instructions, cond_value) <- translateExpression condExpr
      else_label <- newId "cond.else"
      end_label <- newId "cond.end"
      (then_instructions, then_value) <- translateExpression thenExpr
      (else_instructions, else_value) <- translateExpression elseExpr
      varid <- newId "tmp.cond"
      let destination = Variable varid
      let instructions =
            cond_instructions
              ++ [JumpIfZero else_label cond_value]
              ++ then_instructions
              ++ [Copy then_value destination, Jump end_label, Label else_label]
              ++ else_instructions
              ++ [Copy else_value destination, Label end_label]
      return (instructions, destination)

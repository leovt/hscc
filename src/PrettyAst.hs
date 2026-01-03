-- re-export Pretty for convenience
-- ensure instances are available
{-# OPTIONS_GHC -Wno-orphans #-}

module PrettyAst
  ( module Prettyprinter,
  )
where

import Data.List (intercalate)
import Parser
import Prettyprinter (Doc, Pretty (pretty), hardline, nest, parens, pretty, vsep, (<+>), (<>))

instance Pretty Program where
  pretty (Program f) = pretty f

instance Pretty Function where
  pretty (Function name params (Just block) storage_class) =
    pretty "Function"
      <+> pretty name
      <+> pretty (show storage_class)
      <+> parens (pretty (intercalate ", " params))
      <+> pretty block
  pretty (Function name params Nothing storage_class) =
    pretty "Function"
      <+> pretty name
      <+> pretty (show storage_class)
      <+> parens (pretty (intercalate ", " params))

instance Pretty Declaration where
  pretty (FunctionDeclaration fun) = pretty fun
  pretty decl@(VariableDeclaration {}) = pretty (show decl)

instance Pretty BlockItem where
  pretty (Stmt stmt) = pretty stmt
  pretty (Decl decl) = pretty decl

instance Pretty Block where
  pretty (Block items) =
    pretty "{"
      <> hardline
      <> vsep (map (nest 2 . pretty) items)
      <> hardline
      <> pretty "}"

instance Pretty Statement where
  pretty (CompoundStatement block) = pretty block
  pretty (SwitchStatement expr block) =
    pretty "switch" <+> parens (pretty (show expr)) <+> pretty block
  pretty (ForStatement init cond incr stmt) =
    pretty "for"
      <+> parens
        ( prettyMaybeInit init
            <> pretty ";"
            <+> prettyMaybeExpression cond
            <> pretty ";"
            <+> prettyMaybeExpression incr
        )
      <+> pretty stmt
    where
      prettyMaybeExpression Nothing = pretty ""
      prettyMaybeExpression (Just e) = pretty (show e)
      prettyMaybeInit Nothing = pretty ""
      prettyMaybeInit (Just (ForInitDecl decl)) = pretty (show decl)
      prettyMaybeInit (Just (ForInitExpr expr)) = pretty expr
  pretty (WhileStatement cond stmt) =
    pretty "while" <+> parens (pretty cond) <+> pretty stmt
  pretty (DoWhileStatement cond stmt) =
    pretty "DoWhileStatement" <+> pretty stmt <+> hardline <> pretty "while" <+> parens (pretty cond)
  pretty (IfStatement cond thenStmt Nothing) =
    pretty "if" <+> parens (pretty cond) <+> pretty thenStmt
  pretty (IfStatement cond thenStmt (Just elseStmt)) =
    pretty "if"
      <+> parens (pretty cond)
      <+> pretty thenStmt
      <+> pretty "else"
      <+> pretty elseStmt
  pretty (LabelledStatement label stmt) =
    pretty (show label) <> pretty ":" <> hardline <> pretty stmt
  pretty (ExpressionStatement expr) =
    pretty expr <> pretty ";"
  pretty stmt = pretty (show stmt)

instance Pretty Expression where
  pretty expr = pretty (show expr)
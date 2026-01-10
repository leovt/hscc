-- re-export Pretty for convenience
-- ensure instances are available
{-# OPTIONS_GHC -Wno-orphans #-}

module PrettyAst
  ( module Prettyprinter,
  )
where

import CTypes (CType)
import Data.List (intercalate)
import qualified Data.Map as Map
import Parser
import Prettyprinter (Doc, Pretty (pretty), brackets, hardline, nest, parens, pretty, vsep, (<+>), (<>))
import Validate (SymbolInfo (..), SymbolTable (..))

instance Pretty Program where
  pretty (Program f) = pretty f

instance Pretty FunctionDeclaration where
  pretty (FunctionDeclaration name params (Just block) storage_class scope) =
    pretty "Function"
      <+> pretty name
      <+> brackets (pretty (show storage_class) <+> pretty (show scope))
      <+> parens (pretty (intercalate ", " (map showParam params)))
      <+> pretty block
  pretty (FunctionDeclaration name params Nothing storage_class scope) =
    pretty "Function"
      <+> pretty name
      <+> brackets (pretty (show storage_class) <+> pretty (show scope))
      <+> parens (pretty (intercalate ", " (map showParam params)))

showParam :: (CType, Maybe String) -> String
showParam (ctype, Just name) = show ctype ++ " " ++ name
showParam (ctype, Nothing) = show ctype

instance Pretty Declaration where
  pretty (FunDecl fun) = pretty fun
  pretty decl@(VarDecl {}) = pretty (show decl)

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

instance Pretty SymbolInfo where
  pretty (SymbolInfo t attrs) = pretty (show t ++ " " ++ show attrs)

instance Pretty SymbolTable where
  pretty (SymbolTable m) =
    let entries = Map.toList m
        prettyEntry (name, info) = pretty name <> pretty ":" <+> pretty info
     in vsep (map prettyEntry entries)

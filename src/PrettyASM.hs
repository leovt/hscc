-- re-export Pretty for convenience
-- ensure instances are available
{-# OPTIONS_GHC -Wno-orphans #-}

module PrettyASM
  ( module Prettyprinter,
  )
where

import AsmAst
import Prettyprinter (Doc, Pretty (pretty), hardline, nest, pretty, sep, vsep, (<+>), (<>))

instance Pretty Program where
  pretty (Program f) = pretty f

instance Pretty TopLevel where
  pretty (Function name global instrs) =
    pretty "Function"
      <+> pretty name
      <+> pretty (if global then "(global)" else "(local)")
      <+> pretty "{"
      <> hardline
      <> vsep (map pretty instrs)
      <> hardline
      <> pretty "}"
  pretty other = pretty (show other)

instance Pretty Instruction where
  -- labels are unindented; other instructions are shown indented
  pretty (Label name) = pretty name <> pretty ":"
  pretty instr = pretty "    " <> pretty (show instr)

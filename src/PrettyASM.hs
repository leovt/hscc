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

instance Pretty Function where
  pretty (Function name instrs) =
    pretty "Function"
      <+> pretty name
      <+> pretty "{"
      <> hardline
      <> vsep (map pretty instrs)
      <> hardline
      <> pretty "}"

instance Pretty Instruction where
  -- labels are unindented; other instructions are shown indented
  pretty (Label name) = pretty name <> pretty ":"
  pretty instr = pretty "    " <> pretty (show instr)

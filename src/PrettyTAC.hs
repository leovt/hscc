-- re-export Pretty for convenience
-- ensure instances are available
{-# OPTIONS_GHC -Wno-orphans #-}

module PrettyTAC
  ( module Prettyprinter,
  )
where

import Prettyprinter (Doc, Pretty (pretty), hardline, nest, pretty, vsep, (<+>), (<>))
import TAC

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
  -- use the default `show` representation for instructions
  pretty (Label name) = pretty name <> pretty ":"
  pretty instr = pretty "    " <> pretty (show instr)

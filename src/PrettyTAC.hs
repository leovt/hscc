-- re-export Pretty for convenience
-- ensure instances are available
{-# OPTIONS_GHC -Wno-orphans #-}

module PrettyTAC
  ( module Prettyprinter,
  )
where

import Data.List (intercalate)
import Prettyprinter (Doc, Pretty (pretty), hardline, nest, parens, pretty, vsep, (<+>), (<>))
import TAC

instance Pretty Program where
  pretty (Program f) = pretty f

instance Pretty Function where
  pretty (Function name args instrs) =
    pretty "Function"
      <+> pretty name
      <+> parens (pretty $ intercalate ", " (map show args))
      <+> pretty "{"
      <> hardline
      <> vsep (map pretty instrs)
      <> hardline
      <> pretty "}"

instance Pretty Instruction where
  -- use the default `show` representation for instructions
  pretty (Label name) = pretty name <> pretty ":"
  pretty instr = pretty "    " <> pretty (show instr)

module CTypes
  ( CType (..),
  )
where

data CType
  = IntT
  | FuncT CType [CType]
  deriving (Eq, Show)

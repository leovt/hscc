module CTypes
  ( CType (..),
  )
where

data CType
  = IntT
  | LongIntT
  | FuncT CType [CType]
  deriving (Eq, Show)

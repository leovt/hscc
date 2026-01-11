module CTypes
  ( CType (..),
    commonType,
    isIntegralType,
  )
where

data CType
  = IntT
  | LongIntT
  | FuncT CType [CType]
  deriving (Eq, Show)

commonType :: CType -> CType -> Maybe CType
commonType IntT IntT = Just IntT
commonType IntT LongIntT = Just LongIntT
commonType LongIntT IntT = Just LongIntT
commonType LongIntT LongIntT = Just LongIntT
commonType _ _ = Nothing

isIntegralType :: CType -> Bool
isIntegralType IntT = True
isIntegralType LongIntT = True
isIntegralType _ = False
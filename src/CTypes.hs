module CTypes
  ( CType (..),
    commonType,
    isIntegralType,
    truncateIntegral,
  )
where

import Data.Bits (Bits (shiftL))

data CType
  = IntT
  | LongIntT
  | UIntT
  | ULongIntT
  | FuncT CType [CType]
  deriving (Eq, Show)

commonType :: CType -> CType -> Maybe CType
commonType IntT IntT = Just IntT
commonType LongIntT LongIntT = Just LongIntT
commonType UIntT UIntT = Just UIntT
commonType ULongIntT ULongIntT = Just ULongIntT
commonType IntT LongIntT = Just LongIntT
commonType LongIntT IntT = Just LongIntT
commonType UIntT ULongIntT = Just ULongIntT
commonType ULongIntT UIntT = Just ULongIntT
commonType IntT UIntT = Just UIntT
commonType UIntT IntT = Just UIntT
commonType _ ULongIntT = Just ULongIntT
commonType ULongIntT _ = Just ULongIntT
commonType UIntT LongIntT = Just LongIntT
commonType LongIntT UIntT = Just LongIntT
commonType _ _ = Nothing

isIntegralType :: CType -> Bool
isIntegralType IntT = True
isIntegralType LongIntT = True
isIntegralType UIntT = True
isIntegralType ULongIntT = True
isIntegralType _ = False

truncateIntegral :: CType -> Integer -> Integer
truncateIntegral IntT n = (n + (1 `shiftL` 31)) `mod` (1 `shiftL` 32) - (1 `shiftL` 31)
truncateIntegral LongIntT n = (n + (1 `shiftL` 63)) `mod` (1 `shiftL` 64) - (1 `shiftL` 63)
truncateIntegral _ _ = error "truncateIntegral: not an integral type"
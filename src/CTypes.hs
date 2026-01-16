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

truncateIntegral :: CType -> Integer -> Integer
truncateIntegral IntT n = (n + (1 `shiftL` 31)) `mod` (1 `shiftL` 32) - (1 `shiftL` 31)
truncateIntegral LongIntT n = (n + (1 `shiftL` 63)) `mod` (1 `shiftL` 64) - (1 `shiftL` 63)
truncateIntegral _ _ = error "truncateIntegral: not an integral type"
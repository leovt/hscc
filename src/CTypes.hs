module CTypes
  ( CType (..),
    commonType,
    isIntegralType,
    isScalarType,
    isPointerType,
    truncateIntegral,
    intT,
    longIntT,
    uIntT,
    uLongIntT,
    funcT,
    doubleT,
    IntegralType (..),
    Signed (..),
    IntSize (..),
    ArithmeticType (..),
  )
where

import Data.Bits (Bits (shiftL))

data CType
  = ArithmeticType ArithmeticType
  | FuncT CType [CType]
  | Pointer CType
  deriving (Eq, Show)

data Signed = Signed | Unsigned
  deriving (Show, Eq)

data IntSize = Int | Long
  deriving (Show, Eq)

data IntegralType = IType Signed IntSize
  deriving (Show, Eq)

data ArithmeticType
  = Integral IntegralType
  | DoubleType
  deriving (Show, Eq)

{- helper constructors -}
intT :: CType
intT = ArithmeticType (Integral (IType Signed Int))

longIntT :: CType
longIntT = ArithmeticType (Integral (IType Signed Long))

uIntT :: CType
uIntT = ArithmeticType (Integral (IType Unsigned Int))

uLongIntT :: CType
uLongIntT = ArithmeticType (Integral (IType Unsigned Long))

funcT :: CType -> [CType] -> CType
funcT = FuncT

doubleT :: CType
doubleT = ArithmeticType DoubleType

commonType :: CType -> CType -> Maybe CType
commonType (ArithmeticType t1@(Integral (IType s1 sz1))) (ArithmeticType t2@(Integral (IType s2 sz2)))
  | t1 == t2 = Just (ArithmeticType t1)
  | otherwise =
      case (s1, sz1, s2, sz2) of
        (Unsigned, Long, _, _) -> Just uLongIntT
        (_, _, Unsigned, Long) -> Just uLongIntT
        (_, Int, Signed, Long) -> Just longIntT
        (Signed, Long, _, Int) -> Just longIntT
        (Unsigned, Int, Signed, Int) -> Just uIntT
        (Signed, Int, Unsigned, Int) -> Just uIntT
        _ -> Nothing
commonType (ArithmeticType DoubleType) (ArithmeticType _) = Just doubleT
commonType (ArithmeticType _) (ArithmeticType DoubleType) = Just doubleT
commonType _ _ = Nothing

isIntegralType :: CType -> Bool
isIntegralType (ArithmeticType (Integral _)) = True
isIntegralType _ = False

isScalarType :: CType -> Bool
isScalarType (ArithmeticType _) = True
isScalarType (Pointer _) = True
isScalarType _ = False

isPointerType :: CType -> Bool
isPointerType (Pointer _) = True
isPointerType _ = False

truncateIntegral :: IntegralType -> Integer -> Integer
truncateIntegral (IType Signed Int) n = (n + (1 `shiftL` 31)) `mod` (1 `shiftL` 32) - (1 `shiftL` 31)
truncateIntegral (IType Signed Long) n = (n + (1 `shiftL` 63)) `mod` (1 `shiftL` 64) - (1 `shiftL` 63)
truncateIntegral (IType Unsigned Int) n = n `mod` (1 `shiftL` 32)
truncateIntegral (IType Unsigned Long) n = n `mod` (1 `shiftL` 64)

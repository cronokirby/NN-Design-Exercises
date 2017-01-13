{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Vector where

{- Activation functions -}

hardLim :: Double -> Double
hardLim n
    | n < 0  = 0
    | n >= 0 = 1

hardLims :: Double -> Double
hardLims n
    | n < 0  = -1
    | n >= 0 = 1

pureLin :: Double -> Double
pureLin n = n

satLin :: Double -> Double
satLin n
    | n < 0     = 0
    | n > 1     = 1
    | otherwise = n

satLins :: Double -> Double
satLins n
    | n < -1    = -1
    | n > 1     = 1
    | otherwise = n

logSig :: Double -> Double
logSig n = 1 / (1 + exp (negate n))

tanSig :: Double -> Double
tanSig n = (power - inverse) / (power + inverse)
    where
      power = exp n
      inverse = exp (negate n)

posLin :: Double -> Double
posLin n
    | n < 0     = 0
    | otherwise = n


{- Vectors -}
type Vector = [Double]
type Matrix = [Vector]

class Linear a b c | a b -> c where
    infix 7 |*|
    (|*|) :: a -> b -> c
    infix 6 |+|
    (|+|) :: a -> b -> a


instance Linear Matrix Vector Vector where
    m |*| v = map (\row -> sum $ zipWith (*) row v) m
    m |+| v = map (\row -> zipWith (+) row v) m

instance Linear Vector Vector Vector where
    -- Mathematically, v2 is converted to a matrix here
    v1 |*| v2 = zipWith (*) v1 v2
    v1 |+| v2 = zipWith (+) v1 v2

infix 6 |-|
(|-|) :: Vector -> Vector -> Vector
v1 |-| v2 = zipWith (-) v1 v2

module NeuralNet where

import Data.List

import Vector

type Activator = Double -> Double
type Weights = Matrix
type Bias = Vector

-- A layer can be seen as a big function from Vector -> Vector
-- When the `input` is omitted, this becomes a curried function
layer :: Weights -> Bias -> Activator -> (Vector -> Vector)
layer weights bias activator input = map activator
                                    (weights |*| input |+| bias)

-- Keeps calling itself until it converges
reccurent :: Weights -> Bias -> Activator -> (Vector -> Vector)
reccurent weights bias activator input
    | input == result = result
    | otherwise       = reccurent weights bias activator result
    where
      result = layer weights bias activator input



{- Full constructs -}

-- Creates a new hamming network, given a matrix of prototypes
hamming :: Matrix -> (Vector -> Vector)
hamming prototypes = layer2 . layer1
  where
    bias = (fromIntegral . length) <$> prototypes
    layer1 = layer prototypes bias pureLin
    sigma = 1 / fromIntegral (length prototypes - 1) - 0.5

    weights2 = [[1, negate sigma],
                [negate sigma, 1]]
    layer2 = reccurent weights2 [0,0] posLin



{- Perceptron networks -}
-- Represents a prototype, with a vector for it's target and value
type ProtoType = (Vector, Vector)
data Perceptron = Perceptron { weights :: Weights
                             , bias :: Bias
                             , activator :: Activator }

apply :: Perceptron -> Vector -> Vector
apply (Perceptron w b a) = layer w b a


-- Uses the perceptron learning rule to modify an existing network
evaluate :: (Perceptron, Int) -> ProtoType -> (Perceptron, Int)
evaluate (p, successCount) (prototype, target) =
    ( Perceptron (weights p |+| err |*| prototype)
                 (bias p |+| err)
                 (activator p)
    , success)
  where
    result = apply p prototype
    err = target |-| result
    success = case sum err of
        0 -> successCount + 1
        _ -> 0


-- Takes the # of classes, and a list of the prototyes
perceptronNet :: Int -> [ProtoType] -> (Vector -> Vector)
perceptronNet classCount prototypes =
    let initialBias    = map (const 0) $ fst $ head prototypes
        initialWeights = map (const initialBias) [1..(classCount `div` 2)]
        initial = (Perceptron initialWeights initialBias hardLim, 0)
        Just (convergent, _) = find (\(_, count) ->
                                      count == length prototypes - 1)
                        $ scanl evaluate initial (cycle prototypes)
    in apply convergent

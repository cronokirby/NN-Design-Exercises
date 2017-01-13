module Exercises where

import Vector
import NeuralNet

{- Exercice 3.1 -}
banana = [-1, 1, -1]
pineapple = [-1, -1, 1]

fruitHamming = hamming [banana, pineapple]

test1 = [1, -1, 1] :: Vector

{- Exercise 4.2 -}
testInputs42 = [[-1, 1], [-1, -1], [0, 0], [1, 0]] :: Matrix
layer42 = layer [[-1.5, 0.5]] [-0.5] hardLim

{- Exercise 4.4 -}
testInputs44 = [ ([-1, 1], [1])
               , ([-1, -1], [1])
               , ([0, 0], [0])
               , ([1, 0], [0])] :: [ProtoType]

test44 = [[-1, 0.5], [0, 0.5]]

p = perceptronNet 2 testInputs44

testoo = [0, 0] :: [Double]

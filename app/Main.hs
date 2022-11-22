module Main where

import MnistDigit

type Value = Double
type Weight = Double
type Inputs = [Value]
type Outputs = [Value]
type Weights = [Weight]
data Neuron = Neuron 
    { neuronWeights :: Weights
    , neuronBais    :: Value
    } deriving (Show)
type Layer = [Neuron]
type Model = [Layer]

avg :: (Foldable t, Fractional a) => t a -> a
avg l = sum l / (fromIntegral $ length l)

runWeights :: Inputs -> Weights -> Value
runWeights i = avg . zipWith (*) i

runNeuron :: Inputs -> Neuron -> Value
runNeuron i n = neuronBais n * (tanh $ runWeights i $ neuronWeights n)

runLayer :: Inputs -> Layer -> Outputs
runLayer = map . runNeuron 

runModel :: Inputs -> Model -> Outputs
runModel = foldl runLayer

main :: IO ()
main = readFile "mnist_test.csv" 
     >>= mapM_ (putStrLn . printMnistDigit) . parseMnistDigits

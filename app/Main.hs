module Main where

import MnistDigit
import System.Random

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

makeWeigths :: Int -> IO Weights
makeWeigths n = mapM (const (randomIO :: IO Double)) [1..n]

makeNeuron :: Int -> IO Neuron
makeNeuron n = do
    weights <- makeWeigths n
    bais    <- randomIO
    pure Neuron 
        { neuronWeights = weights
        , neuronBais    = bais
        }

makeLayer :: Int -> Int -> IO Layer
makeLayer isz lsz = mapM (const $ makeNeuron isz) [1..lsz]

makeModel :: [Int] -> IO Model
makeModel [x,y]    = makeLayer x y >>= pure . pure
makeModel (x:y:xs) = do
    la <- makeLayer x y
    rm <- makeModel $ y:xs
    pure $ la:rm

main :: IO ()
main = readFile "mnist_test.csv" 
     >>= mapM_ (putStrLn . printMnistDigit) . parseMnistDigits

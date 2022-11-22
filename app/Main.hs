module Main where

import MnistDigit

main :: IO ()
main = readFile "mnist_test.csv" 
     >>= mapM_ (putStrLn . printMnistDigit) . parseMnistDigits

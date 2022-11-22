module MnistDigit where

import Data.List.Split

data MnistDigit = MnistDigit
    { mnistDigit :: Int
    , mnistImage :: [Double]
    } deriving Show

parseMnistDigit :: String -> MnistDigit
parseMnistDigit str = 
    let line  = splitOn "," str
        digit =               read  $ head line
        image = map ((/255) . read) $ tail line
     in MnistDigit digit image

parseMnistDigits :: String -> [MnistDigit]
parseMnistDigits = map parseMnistDigit . lines

showImage :: MnistDigit -> String
showImage = unlines
          . map (map ((grayscale !!) . floor . (* (fromIntegral (length grayscale - 1)))))
          . chunksOf 28
          . mnistImage
    where grayscale = reverse "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. "

printMnistDigit :: MnistDigit -> String
printMnistDigit digit = unlines
                      $ (show $ mnistDigit digit) : [showImage digit]

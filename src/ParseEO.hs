module ParseEO where

import Data.Char ( ord, isDigit )

data Board = B [Int]

nextPositions :: Board -> [Board]
nextPositions b = [b]

f n 
    | n < 0 = []
    | otherwise = 

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred = do
    
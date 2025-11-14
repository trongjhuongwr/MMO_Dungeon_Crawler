module Utils.Random 
  ( 
    getRandomNumber
  , getRandomFloat
  ) where

import System.Random (randomRIO)

getRandomNumber :: IO Int
getRandomNumber = randomRIO (1000, 9999) 

-- | Lấy một số Float ngẫu nhiên trong một khoảng
getRandomFloat :: (Float, Float) -> IO Float
getRandomFloat (minVal, maxVal) = randomRIO (minVal, maxVal)
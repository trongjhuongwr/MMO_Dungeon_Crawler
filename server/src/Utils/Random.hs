module Utils.Random where

import System.Random (randomRIO)

getRandomNumber :: IO Int
getRandomNumber = randomRIO (1000, 9999) 
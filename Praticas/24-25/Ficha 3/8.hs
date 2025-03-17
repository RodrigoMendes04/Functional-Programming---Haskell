--module Main where

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [x..n], x^2 + y^2 == z^2]

-- main :: IO ()
-- main = print (pythagorean 10)
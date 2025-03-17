prime :: Integer -> Bool
prime n = divprop n == [1]

divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]
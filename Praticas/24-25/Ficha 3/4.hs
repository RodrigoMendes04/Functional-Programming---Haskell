perfect :: Integer -> [Integer]
perfect n = [x | x<-[1..n], x == sum (divprop x)]

divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]

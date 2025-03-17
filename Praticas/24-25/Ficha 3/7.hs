dotprod :: [Float] -> [Float] -> Float
dotprod xs ys = sum [x * y | (x, y) <- zip xs ys]
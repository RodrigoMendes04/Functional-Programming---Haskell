aprox :: Int -> Double
aprox n = 4 * sum [((-1) ** fromIntegral k) / fromIntegral (2 * k + 1) | k <- [0..n-1]]

aprox' :: Int -> Double
aprox' n = sqrt (12 * sum [((-1) ** fromIntegral k) / fromIntegral ((k + 1) ^ 2) | k <- [0..n-1]])

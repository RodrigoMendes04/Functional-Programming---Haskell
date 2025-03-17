max', min' :: Ord a => a -> a -> a
max' x y = if x >= y then x else y
min' x y = if x <= y then x else y

max3' :: Ord a => a -> a -> a -> a
max3' x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z

min3' :: Ord a => a -> a -> a -> a
min3' x y z
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | otherwise = z

max3 :: Ord a => a -> a -> a -> a
max3 x y z = max' (max' x y) z

min3 :: Ord a => a -> a -> a -> a
min3 x y z = min' (min' x y) z

-- Ex.1
incr :: Int -> Int
incr x = x + 1

triple :: Int -> Int
triple x = 3 * x

welcome :: String -> String
welcome name = "Hello, " ++ name ++ "!"

count :: String -> String
count str = show (length str) ++ " characters."

-- Ex.2
leftHalf :: [a] -> [a]
leftHalf xs = take (length xs `div` 2) xs

rightHalf :: [a] -> [a]
rightHalf xs = drop (length xs `div` 2) xs

-- Ex.3
-- a)
second :: [a] -> a
second xs = head (tail xs)

-- b)
getLast :: [a] -> a
getLast [x] = x
getLast (_:xs) = getLast xs

-- c)
init2 :: [a] -> [a]
init2 [_] = []
init2 (x:xs) = x : init2 xs

-- d)
middle :: [a] -> a
middle [] = error "Lista vazia"
middle [x] = x
middle xs
    | odd (length xs) = xs !! midIndex -- O odd é uma função em Haskell que retorna True se o número passado como argumento for ímpar e False se for par. Ele é aplicado a um número inteiro e retorna um valor do tipo Bool
    | otherwise = error "Lista com tamanho par"
    where midIndex = length xs `div` 2

-- e)
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome word = word == reverse word

-- Ex.4
checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = a < b + c && b < a + c && c < a + b

-- Ex.5
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt(s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

-- 5.2
primo :: Integer -> Bool
primo n | n < 2 = False
        | otherwise = all (\x -> n `mod` x /= 0) [2..floor(sqrt(fromIntegral n))]

-- 5.3
-- a)
-- esconder a definição do prelúdio 
import Prelude hiding ((++))

-- completar esta definição
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr f z lista
    where f     = (:)
          z     = ys
          lista = xs

-- c)
-- esconder a definição do prelúdio 
import Prelude hiding (reverse)

-- completar esta definição
reverse :: [a] -> [a]
reverse xs = foldr f z xs
    where f     = (\x acc -> acc ++ [x])
          z     = []
		 
-- 5.5
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


-- 5.7 (esta algo de errado)
palavras :: String -> [String]
palavras "" = []
palavras str =
    let word = takeWhile (/= ' ') str  -- obter a palavra atual usando takeWhile
        remaining = dropWhile (== ' ') (dropWhile (/= ' ') str)  -- remover a palavra atual + os espaços em branco seguintes
    in if null word
        then palavras remaining
        else word : palavras remaining  -- adiciona a palavra à lista e continua para a próxima palavra

-- 5.9 (esta mal tambem)
-- Definição da função aproxPi1
aproxPi1 :: Int -> Double
aproxPi1 n = sum $ take n $ zipWith3 (\x y z -> x / y * z) (cycle [4, -4]) [1,3..] (cycle [1])

-- Definição da função aproxPi2 (está a dar errado)
aproxPi2 :: Int -> Double
aproxPi2 n = 3 + sum [(((-1)^^i)*(4/((2*fromIntegral i+1)*(2*fromIntegral i+2)*(2*fromIntegral i+3)))) | i <- [1..n]]

-- 5.10
-- a)
binom :: Integer -> Integer -> Integer
binom _ 0 = 1
binom n k
    | n == k = 1
    | otherwise = binom (n - 1) (k - 1) * n `div` k

-- Definição de pascal usando a função binom
pascal :: [[Integer]]
pascal = [[binom n k | k <- [0..n]] | n <- [0..]]

-- b)
-- Definição alternativa de pascal sem cálculo de fatoriais
pascal2 :: [[Integer]]
pascal2 = iterate nextRow [1]
    where
    nextRow row = zipWith (+) (row ++ [0]) ([0] ++ row)

-- 5.11
-- Função que gera uma lista infinita de números primos
primes :: [Integer]
primes = sieve [2..]
    where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Função que encontra uma testemunha da Conjectura de Goldbach para um inteiro par n
goldbach :: Integer -> (Integer, Integer)
goldbach n
    | n <= 2 || odd n = error "Input inválido. Forneça um número par maior que 2."
    | otherwise = head [(p, n - p) | p <- takeWhile (<= n `div` 2) primes, isPrime (n - p)]
    where
        isPrime x = x > 1 && null [d | d <- [2..floor (sqrt (fromIntegral x))], x `mod` d == 0]


-- 5.13
-- a)
import Data.Char

-- Função para cifrar uma mensagem usando a cifra de Vigenère
vigenere :: String -> String -> String
vigenere keyword message = zipWith shiftChar (cycle keyword) message
    where
    shiftChar k m
        | isLetter m = chr $ (ord 'A' + (ord m - ord 'A' + shiftAmount k) `mod` 26)
        | otherwise = m
    shiftAmount k = ord (toUpper k) - ord 'A'

-- Função para decifrar uma mensagem cifrada usando a cifra de Vigenère
unVigenere :: String -> String -> String
unVigenere keyword cipherText = zipWith shiftBackChar (cycle keyword) cipherText
    where
    shiftBackChar k c
        | isLetter c = chr $ (ord 'A' + (ord c - ord 'A' - shiftAmount k) `mod` 26)
        | otherwise = c
    shiftAmount k = ord (toUpper k) - ord 'A'

-- Exemplo de uso
main :: IO ()
main = do
    let keyword = "LUAR"
        message = "ATAQUEDEMADRUGADA"
        cipherText = vigenere keyword message
        decodedText = unVigenere keyword cipherText
    putStrLn $ "Mensagem cifrada: " ++ cipherText
    putStrLn $ "Mensagem decifrada: " ++ decodedText




main :: IO ()
main = do
    print $ palavras "Abra- ca- dabra!"
    print $ palavras " cadabra! "
    print $ palavras " "
    -- Calcular uma aproximação de π usando 100 termos da Série 1
    let piApproximation1 = aproxPi1 100
    putStrLn $ "Aproximação de π usando 100 termos da Série 1: " ++ show piApproximation1
    -- Calcular uma aproximação de π usando 100 termos da Série 2
    let piApproximation2 = aproxPi2 100
    putStrLn $ "Aproximação de π usando 100 termos da Série 2: " ++ show piApproximation2
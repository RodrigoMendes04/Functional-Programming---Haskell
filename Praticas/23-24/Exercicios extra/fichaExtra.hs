-- LISTAS INFINITAS

-- Listar números primos

primos :: [Integer]
primos = crivo [2..]
  where
    crivo (p:xs) = p : filter (\x -> x `mod` p /= 0) (crivo xs)

listarPrimos :: Integer -> Integer -> [Integer]
listarPrimos a b = takeWhile (<= b) . dropWhile (< a) $ primos

-- Listar primos gemeos

primos :: [Integer]
primos = crivo [2..]
  where\ 
    crivo (p:xs) = p : filter (\x -> x `mod` p /= 0) (crivo xs)

gemeos :: [(Integer, Integer)]
gemeos = [(p1, p2) | (p1, p2) <- zip primos (tail primos), p2 - p1 == 2]

-- GRAFOS E RELAÇÕES

-- Testar transitividade de um grafo

type G = ([V], [E])  -- grafo
type V = Int         -- vértices (inteiros)
type E = (Int, Int)  -- arcos (pares de inteiros)

transitiva :: G -> Bool
transitiva (vs, es) = all (\(a, c) -> (a, c) `elem` es) transitivePairs
  where
    transitivePairs = [(a, c) | (a, b1) <- es, (b2, c) <- es, b1 == b2]

-- Fecho transitivo de um grafo

import Data.List (nub)

type G = ([V], [E])
type V = Int
type E = (Int, Int)

fechoT :: G -> G
fechoT (vs, es) = (vs, fecho es)
  where
    fecho es
      | newEdges == es = es
      | otherwise = fecho newEdges
      where
        newEdges = nub (es ++ [(a, c) | (a, b1) <- es, (b2, c) <- es, b1 == b2])

-- PILHAS

-- Testar parêntesis equilibrados

type Stack = [Char]

parent :: String -> Bool
parent s = verifica s []

verifica :: String -> Stack -> Bool
verifica [] [] = True  -- Se a sequência acabou e a pilha está vazia, está balanceado
verifica [] _ = False  -- Se a sequência acabou e a pilha não está vazia, não está balanceado
verifica (c:cs) stack
  | c `elem` "([{`" = verifica cs (c:stack)  -- Se é um parêntesis de abertura, empilha
  | c `elem` ")]}`" = not (null stack) && corresponde (head stack) c && verifica cs (tail stack)  -- Se é de fechamento, verifica o topo da pilha
  | otherwise = False  -- Se for um caractere inválido, retorna False

corresponde :: Char -> Char -> Bool
corresponde '(' ')' = True
corresponde '[' ']' = True
corresponde '{' '}' = True
corresponde _ _ = False

-- Calculadora RPN

import Data.List (foldl')

calcular :: String -> Integer
calcular expr = head $ foldl' processa [] (words expr)
  where
    processa :: [Integer] -> String -> [Integer]
    processa stack token
      | token == "+" = let (x:y:ys) = stack in (y + x) : ys
      | token == "-" = let (x:y:ys) = stack in (y - x) : ys
      | token == "*" = let (x:y:ys) = stack in (y * x) : ys
      | token == "/" = let (x:y:ys) = stack in (y `div` x) : ys
      | otherwise    = read token : stack
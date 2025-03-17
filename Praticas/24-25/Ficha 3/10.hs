--module Main where

power :: (Fractional a, Integral b) => a -> b -> a
power x n
    | n >= 0    = product [x | _ <- [1..n]]  -- Caso n positivo ou zero
    | otherwise = 1 / product [x | _ <- [1..(-n)]]  -- Caso n negativo

--main :: IO ()
--main = print (power (-2) 2)

-- Caso n >= 0:
   --Criamos uma lista com n cópias de x, como [x, x, ..., x].
   --Multiplicamos todos os elementos da lista (product [...]), o que equivale a x^n
--Caso n < 0:
  --Calculamos x^(-n)
  --Geramos a potência positiva com product e depois invertemos (1 / ...).
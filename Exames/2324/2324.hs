-- 1. Intercalar
intercalar :: Int -> [Int] -> [Int]
intercalar x [y] = [y]
intercalar x [y,ys] = [y,x,ys]
intercalar x (y:ys) = y : x : intercalar x ys

-- 2. Função para artan x = x - x^3/3 + x^5/5 - x^7/7 + ...

aproxAtan :: Int -> Double -> Double
aproxAtan n x = sum terms
    where
        terms = [((-1) ^ k) * (x ^ (2 * k + 1)) / fromIntegral (2 * k + 1) | k <- [0..n]]

-- ((-1) ^ k) alterna o sinal dos termos.
-- (x ^ (2 * k + 1)) calcula a potência de x.
-- fromIntegral (2 * k + 1) converte 2 * k + 1 para Double para a operação de divisão.

-- 3. Filtrar valores de uma árvore binária de pesquisa
-- Ex: filterArv (/=4) (No 3(No 2 Vazio Vazio Vazio) (No 4 Vazio Vazio))
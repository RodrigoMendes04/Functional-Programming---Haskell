binom :: Integer -> Integer -> Integer
binom n k = factorial n `div` (factorial k * factorial (n - k))
    where
        factorial x = product [1..x]

-- factorial x = product [1..x]: função local para calcular o fatorial de um número x. A função product serve para calcular o produto de
-- todos os números de 1 a x, o que resulta no fatorial de x.
-- binom n k = factorial n div (factorial k * factorial (n - k)): Aqui, usamos a fórmula do coeficiente binomial para calcular o valor. Calculamos os
-- fatoriais de n, k e n - k, e em seguida usamos a fórmula para encontrar o resultado.

pascal :: Integer -> [[Integer]]
pascal n = map (\x -> map (\k -> binom x k) [0..x]) [0..n]

-- map (\x -> map (\k -> binom x k) [0..x]) [0..n]: Usamos duas funções map aninhadas para gerar o triângulo de Pascal.
-- A função mais externa map (\x -> ...) mapeia sobre os números de 0 até n, representando cada linha do triângulo de Pascal.
-- Para cada valor x (representando a linha atual), a função interna map (\k -> binom x k) [0..x] mapeia sobre os números de 0 até x, representando cada elemento na linha x.
-- Para cada valor k, calculamos o coeficiente binomial de x e k usando a função binom.
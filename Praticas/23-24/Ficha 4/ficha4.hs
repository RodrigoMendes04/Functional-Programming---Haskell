-- 4.1
algarismos :: Int -> [Int]
algarismos n = reverse (algarismosRev n)

algarismosRev :: Int -> [Int]
algarismosRev 0 = []  -- Caso base: número zero, retorna lista vazia
algarismosRev n = n `mod` 10 : algarismosRev (n `div` 10)  -- Adiciona o algarismo das unidades e recursivamente processa o restante dos algarismos
-- 4.2
toBits :: Int -> [Int]
toBits n = reverse (toBitsRev n)

toBitsRev :: Int -> [Int]
toBitsRev 0 = [0]  -- Caso base: número zero, retorna lista com um único zero
toBitsRev 1 = [1]  -- Caso especial: número um, retorna lista com um único um
toBitsRev n = n `mod` 2 : toBitsRev (n `div` 2)  -- Adiciona o bit menos significativo e recursivamente processa o restante dos bits
-- 4.3
fromBitsRev :: [Int] -> Int
fromBitsRev bits = fromBits (reverse bits)

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (bit:bits) = bit * 2^i + fromBits bits
  where
    i = length bits  -- Calcula o expoente correspondente ao bit atual, onde o bit mais à direita tem expoente 0

-- 4.4
mdc :: Integer -> Integer -> Integer
mdc a 0 = a  -- Caso base: Se b for zero, o MDC é a
mdc a b = mdc b (a `mod` b)  -- Caso geral: Calcula o MDC entre b e o resto da divisão de a por b

-- 4.5
-- a)
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]  -- Caso base: Se a lista estiver vazia, apenas insere o elemento x
insert x (y:ys)
    | x <= y    = x : y : ys  -- Se x for menor ou igual a y, insere x antes de y
    | otherwise = y : insert x ys  -- Caso contrário, insere x recursivamente na cauda da lista

-- b)
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)


-- 4.6
-- a)
minimo :: Ord a => [a] -> a
minimo [x] = x  -- Caso base: Se a lista tiver apenas um elemento, esse elemento é o mínimo
minimo (x:xs) = min x (minimo xs)  -- Compara o primeiro elemento com o mínimo dos restantes

-- b)
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []  -- Se a lista estiver vazia, retorna a lista vazia
delete y (x:xs)
    | y == x    = xs  -- Se o elemento atual for igual ao elemento a ser removido, retornamos a cauda da lista
    | otherwise = x : delete y xs  -- Caso contrário, mantemos o elemento atual e continuamos pelo resto da lista

-- c)
ssort :: Ord a => [a] -> [a]
ssort [] = []  -- Caso base: lista vazia já está ordenada
ssort xs = minElem : ssort (delete minElem xs)
    where minElem = minimo xs

-- 4.7
-- a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys  -- Se a primeira lista estiver vazia, retorna a segunda lista
merge xs [] = xs  -- Se a segunda lista estiver vazia, retorna a primeira lista
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)  -- Se o elemento de xs for menor ou igual ao elemento de ys, colocamos x na frente e continuamos a fusão
    | otherwise = y : merge (x:xs) ys  -- Caso contrário, colocamos y na frente e continuamos a fusão

-- b)
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort leftHalf) (msort rightHalf)  -- Junta as listas ordenadas das duas metades
    where
        (leftHalf, rightHalf) = metades xs  -- Divide a lista em duas metades
        metades xs = splitAt (length xs `div` 2) xs  -- Função auxiliar para dividir a lista em duas metades

-- 4.8
trocos :: Int -> [Int]
trocos 0 = []  -- Caso base: se a quantia for zero, retorna uma lista vazia
trocos valor
    | null moedaFiltro = error "Não é possível representar a quantia com as moedas disponíveis."
    | otherwise = maiorMoeda : trocos (valor - maiorMoeda)
    where
        moedas = [200, 100, 50, 20, 10, 5]
        moedaFiltro = filter (<= valor) moedas
        maiorMoeda = head moedaFiltro

-- filter (<= valor) moedas: cria uma nova lista contendo apenas os elementos de moedas que são menores ou iguais a valor.
-- head: Retorna o primeiro elemento dessa nova lista


myand :: [Bool] -> Bool
myand [] = True
myand (x:xs)
  | x == False = False
  | otherwise = myand xs


  myor :: [Bool] -> Bool
  myor [] = False
  myor (x:xs)
    | x == True = True
    | otherwise = myor xs

-- (c) concat ∶∶ [[a]] → [a] — concatenar uma lista de listas
myConcat :: [[a]] -> [a]
myConcat [] = []  -- Caso base: lista de listas vazia, retorna uma lista vazia
myConcat (x:xs) = x ++ myConcat xs  -- Concatena a primeira lista com a concatenação recursiva do restante

-- (d) replicate ∶∶ Int → a → [a] — produzir uma lista com n elementos iguais
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []  -- Caso base: número de repetições é 0, retorna uma lista vazia
myReplicate n x = x : myReplicate (n - 1) x  -- Adiciona o elemento x à lista e recursivamente replica (n - 1) vezes

-- (e) (!!) ∶∶ [a] → Int → a — selecionar o n-ésimo elemento duma lista
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x  -- Caso base: índice é 0, retorna o primeiro elemento da lista
(!!!) (x:xs) n = xs !!! (n - 1)  -- Recursivamente busca o (n - 1)-ésimo elemento na cauda da lista

-- (f) elem ∶∶ Eq a ⇒ a → [a] → Bool — testar se um valor ocorre numa lista
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False  -- Caso base: lista vazia, retorna False, pois o elemento não está presente
myElem y (x:xs)
    | x == y = True  -- Se o primeiro elemento for igual ao elemento buscado, retorna True
    | otherwise = myElem y xs  -- Caso contrário, verifica recursivamente o restante da lista

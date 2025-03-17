maxIndex :: Ord a => [a] -> (a, Int)
maxIndex [] = error "Lista vazia"
maxIndex (x:xs) = maxIndexHelper xs 1 x 0

maxIndexHelper :: Ord a => [a] -> Int -> a -> Int -> (a, Int)
maxIndexHelper [] _ maxVal maxIndex = (maxVal, maxIndex)
maxIndexHelper (x:xs) currentIndex maxVal maxIndex
    | x > maxVal = maxIndexHelper xs (currentIndex + 1) x currentIndex
    | x == maxVal = maxIndexHelper xs (currentIndex + 1) x currentIndex
    | otherwise = maxIndexHelper xs (currentIndex + 1) maxVal maxIndex

hondt :: Int -> [Int] -> [Int]
hondt numDeputados votos = hondtAux numDeputados votos (replicate (length votos) 0)

hondtAux :: Int -> [Int] -> [Int] -> [Int]
hondtAux 0 _ mandatos = mandatos
hondtAux numDeputados votos mandatos
    | numDeputados > 0 =
        let maxIndex' = snd $ maxIndex $ zipWith (\v m -> fromIntegral v / fromIntegral (m + 1)) votos mandatos
            newMandatos = take maxIndex' mandatos ++ [mandatos !! maxIndex' + 1] ++ drop (maxIndex' + 1) mandatos
        in hondtAux (numDeputados - 1) votos newMandatos
    | otherwise = mandatos
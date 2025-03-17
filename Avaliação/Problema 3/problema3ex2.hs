wordsThatFit :: Int -> [String] -> Int
wordsThatFit maxLength wordsList = length $ takeWhile (\(_, len) -> len <= maxLength) $ zip [1..] $ scanl (\acc word -> acc + length word + 1) 0 wordsList

fillWords :: Int -> [String] -> [[String]]
fillWords maxLength wordsList = go wordsList []
  where
    go [] acc = [reverse acc]
    go (word:rest) acc
      | null acc || length (unwords (acc ++ [word])) <= maxLength = go rest (word : acc)
      | otherwise = reverse acc : go (word:rest) []

main :: IO ()
main = do
    let inputWords = words "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness"
    let maxLength1 = 40
    let maxLength2 = 70
    let result1 = fillWords maxLength1 inputWords
    let result2 = fillWords maxLength2 inputWords
    print result1
    print result2

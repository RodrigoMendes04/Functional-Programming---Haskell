-- 6.1
elefantes :: Int -> IO ()
elefantes n = go 2
    where
        go :: Int -> IO ()
        go m
            | m == n = putStrLn $ "Se " ++ show m ++ " elefantes incomodam muita gente,"
            | otherwise = do
                putStrLn $ "Se " ++ show m ++ " elefantes incomodam muita gente,"
                putStrLn $ show (m + 1) ++ " elefantes incomodam muito mais!"
                go (m + 1)

main :: IO ()
main = elefantes 5

-- 6.2
import System.IO

main :: IO ()
main = do
    texto <- getContents
    let linhas = lines texto
    let palavras = words texto
    let caracteres = length texto
    putStrLn $ show (length linhas)
    putStrLn $ show (length palavras)
    putStrLn $ show caracteres



-- 6.3
import System.IO

main :: IO ()
main = do
    texto <- getContents
    let linhas = lines texto
    let linhasInvertidas = map reverse linhas
    mapM_ putStrLn linhasInvertidas


-- 6.4

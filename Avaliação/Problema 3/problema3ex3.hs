import Data.List (intercalate)

paragraphs :: String -> [String]
paragraphs [] = []
paragraphs text =
  let (paragraph, rest) = breakParagraph text
  in paragraph : paragraphs rest
  where
    breakParagraph :: String -> (String, String)
    breakParagraph [] = ([], [])
    breakParagraph ('\n':'\n':rest) = ([], rest)
    breakParagraph (c:cs) =
      let (para, rem) = breakParagraph cs
      in (c:para, rem)

fillWords :: Int -> [String] -> [[String]]
fillWords maxLength wordsList = go wordsList []
  where
    go [] acc = [reverse acc]
    go (word:rest) acc
      | null acc || length (unwords (acc ++ [word])) <= maxLength = go rest (word : acc)
      | otherwise = reverse acc : go (word:rest) []

main :: IO ()
main = do
    -- Ler a entrada padrão
    inputText <- getContents
    -- Dividir o texto em parágrafos
    let paragraphsList = paragraphs inputText
    -- Formatar cada parágrafo individualmente e juntar as palavras em linhas
    let formattedParagraphs = map (unwords . concat . fillWords 70 . words) paragraphsList
    -- Juntar os parágrafos em linhas
    let formattedText = unlines formattedParagraphs
    -- Imprimir o texto formatado
    putStr formattedText


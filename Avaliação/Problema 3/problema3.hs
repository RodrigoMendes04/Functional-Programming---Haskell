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
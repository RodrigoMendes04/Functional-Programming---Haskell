import Log

parseMessage :: String -> LogEntry
parseMessage line = case words line of
    ("I":timestamp:rest) -> LogMessage Info (read timestamp) (unwords rest)
    ("W":timestamp:rest) -> LogMessage Warning (read timestamp) (unwords rest)
    ("E":level:timestamp:rest) -> LogMessage (Error (read level)) (read timestamp) (unwords rest)
    _ -> Unknown line

main :: IO ()
main = do
    print $ parseMessage "E 2 562 Missed timeout; ignoring"
    print $ parseMessage "I 29 la la la"
    print $ parseMessage "This is not in the right format"

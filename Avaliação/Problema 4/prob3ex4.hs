import Log

insert :: LogEntry -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logEntry Empty = Node logEntry Empty Empty
insert logEntry@(LogMessage _ newTs _) (Node entry@(LogMessage _ ts _) left right)
    | newTs < ts = Node entry (insert logEntry left) right
    | otherwise  = Node entry left (insert logEntry right)

build :: [LogEntry] -> MessageTree
build = foldr insert Empty

inOrder :: MessageTree -> [LogEntry]
inOrder Empty = []
inOrder (Node entry left right) = inOrder left ++ [entry] ++ inOrder right

sortMessages :: [LogEntry] -> [LogEntry]
sortMessages msgs = inOrder (build msgs)

main :: IO ()
main = do
    let log1 = LogMessage Info 10 "First log"
        log2 = LogMessage Warning 20 "Second log"
        log3 = LogMessage (Error 2) 5 "Third log"
        log4 = LogMessage Info 15 "Fourth log"
        unknownLog = Unknown "Unknown log"

    let logs = [log1, log2, log3, log4, unknownLog]

    print $ sortMessages logs

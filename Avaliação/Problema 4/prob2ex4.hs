import Log

insert :: LogEntry -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logEntry Empty = Node logEntry Empty Empty
insert logEntry@(LogMessage _ newTs _) (Node entry@(LogMessage _ ts _) left right)
    | newTs < ts = Node entry (insert logEntry left) right
    | otherwise  = Node entry left (insert logEntry right)

main :: IO ()
main = do
    let log1 = LogMessage Info 10 "First log"
        log2 = LogMessage Warning 20 "Second log"
        log3 = LogMessage (Error 2) 5 "Third log"
        log4 = LogMessage Info 15 "Fourth log"
        unknownLog = Unknown "Unknown log"

    let tree = insert log1 Empty
        tree' = insert log2 tree
        tree'' = insert log3 tree'
        tree''' = insert log4 tree''
        tree'''' = insert unknownLog tree'''

    print tree''''

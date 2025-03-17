module Log where

-- Tipos de mensagens
data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

-- Instante de tempo
type TimeStamp = Int

-- Entrada no ficheiro de log
data LogEntry = LogMessage MessageType TimeStamp String
              | Unknown String
  deriving (Show, Eq)

-- Árvore binária para mensagens
data MessageTree = Empty
                 | Node LogEntry MessageTree MessageTree
  deriving (Show, Eq)

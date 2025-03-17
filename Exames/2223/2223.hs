-- 1
isArmstrong :: Integer -> Bool
isArmstrong n = n == sum (map (^ len) digits)
  where
    digits = map (read . (:[])) (show n)
    len = length digits

-- 2
import Data.Char (toUpper, toLower)

camelCase :: String -> String
camelCase s = concat $ zipWith transform [0..] words s
  where
    transform 0 word = map toLower word
    transform _ word = toUpper (head word) : map toLower (tail word)

-- 3 (a)
import Data.Char (toUpper, toLower)

data Set a = Node a (Set a) (Set a)
           | Empty
           deriving Show

toList :: Set a -> [a]
toList Empty = []
toList (Node x left right) = toList left ++ [x] ++ toList right

isSortedAndUnique :: (Ord a) => [a] -> Bool
isSortedAndUnique [] = True
isSortedAndUnique [_] = True
isSortedAndUnique (x:y:xs) = x < y && isSortedAndUnique (y:xs)

valid :: (Ord a) => Set a -> Bool
valid = isSortedAndUnique . toList

-- 3 (b)
data Set a = Node a (Set a) (Set a)
           | Empty
           deriving Show

-- Função para inserir um elemento na árvore de pesquisa binária
insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | x > y     = Node y left (insert x right)
    | otherwise = Node y left right  -- x == y, não insere duplicata

-- Função para unir dois conjuntos representados como árvores de pesquisa binária
union :: (Ord a) => Set a -> Set a -> Set a
union Empty b = b
union (Node x left right) b = insert x (union (union left right) b)

-- 4
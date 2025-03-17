-- 7.1
import Prop

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Neg p)     = vars p
vars (Conj p q)  = vars p ++ vars q
vars (Disj p q)  = vars p ++ vars q
vars (Impl p q)  = vars p ++ vars q

uniqueVars :: [Char] -> [Char]
uniqueVars [] = []
uniqueVars (x:xs) = x : uniqueVars (filter (/= x) xs)

substitui :: Prop -> [(Char, Bool)] -> Prop
substitui (Const b) _ = Const b
substitui (Var x) vals = case lookup x vals of
                           Just v  -> Const v
                           Nothing -> Var x
substitui (Neg p) vals = Neg (substitui p vals)
substitui (Conj p q) vals = Conj (substitui p vals) (substitui q vals)
substitui (Disj p q) vals = Disj (substitui p vals) (substitui q vals)
substitui (Impl p q) vals = Impl (substitui p vals) (substitui q vals)

eval :: Prop -> Bool
eval (Const b) = b
eval (Var _) = error "Expressão contém variável não substituída"
eval (Neg p) = not (eval p)
eval (Conj p q) = eval p && eval q
eval (Disj p q) = eval p || eval q
eval (Impl p q) = not (eval p) || eval q

boolAssignments :: [Char] -> [[(Char, Bool)]]
boolAssignments [] = [[]]
boolAssignments (x:xs) = [(x, b) : rest | b <- [True, False], rest <- boolAssignments xs]

satisfaz :: Prop -> Bool
satisfaz p = any (\assignment -> eval (substitui p assignment)) assignments
  where varsList = uniqueVars (vars p)
        assignments = boolAssignments varsList

-- 7.4
import Prop

showProp :: Prop -> String
showProp (Const True)  = "T"
showProp (Const False) = "F"
showProp (Var x)       = [x]
showProp (Neg p)       = "(~" ++ showProp p ++ ")"
showProp (Conj p q)    = "(" ++ showProp p ++ " && " ++ showProp q ++ ")"
showProp (Disj p q)    = "(" ++ showProp p ++ " || " ++ showProp q ++ ")"
showProp (Impl p q)    = "(" ++ showProp p ++ " -> " ++ showProp q ++ ")"

-- 7.6
data Arv a = Vazia | No a (Arv a) (Arv a)
  deriving (Show)

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No valor esq dir) = valor + sumArv esq + sumArv dir

-- 7.7
data Arv a = Vazia | No a (Arv a) (Arv a)
  deriving (Show)

listarDecr :: Arv a -> [a]
listarDecr Vazia = []
listarDecr (No valor esq dir) = listarDecr dir ++ [valor] ++ listarDecr esq

-- 7.8
data Arv a = Vazia | No a (Arv a) (Arv a)
  deriving (Show)
<<
nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No valor _ _) = [valor]
nivel n (No _ esq dir) = nivel (n-1) esq ++ nivel (n-1) dir

-- 7.11
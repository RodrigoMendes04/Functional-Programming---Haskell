-- funções auxiliares sobre carateres
import Data.Char(chr, ord)

-- não modifique estas definições!
type Letras = (Char, Char)          -- um bloco de letras
type Digitos = (Int, Int)           -- um bloco de algarismos
type Matricula = (Letras, Digitos, Letras)  -- uma matrícula

valida :: Matricula -> Bool
valida (l1, d, l2) = validaL l1 && validaD d && validaL l2

validaL :: Letras -> Bool
validaL (x1, x2) = 'A' <= x1 && x1 <= 'Z' && 'A' <= x2 && x2 <= 'Z'

validaD :: Digitos -> Bool
validaD (y1, y2) = 0 <= y1 && y1 <= 9 && 0 <= y2 && y2 <= 9
-- complete a definição da função
-- inclua outras funções auxiliares que necessitar
incrMatricula :: Matricula -> Matricula
incrMatricula (l1, d, l2) =
    let l2' = incrBL l2
        d' = if l2' == ('A', 'A') then incrBD d else d
        l1' = if d' == (0, 0) then incrBL l1 else l1
    in (l1', d', l2')
incrBL :: Letras -> Letras
incrBL (a, b)
    | b < 'Z' = (a, chr (ord b + 1))
    | a < 'Z' && b == 'Z' = (chr (ord a + 1), 'A')
    | otherwise = ('A', 'A')
incrBD :: Digitos -> Digitos
incrBD (d1, d2)
    | d2 < 9 = (d1, d2 + 1)
    | d1 < 9 = (d1 + 1, 0)
    | otherwise = (0, 0)

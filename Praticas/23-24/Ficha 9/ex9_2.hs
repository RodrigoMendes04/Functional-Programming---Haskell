Vamos mostrar que
    (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

[] ++ xs = xs
(x:xs) = x ++ xs

Por indução sobre xs

Caso base
xs = []

([] ++ ys) ++ zs =
= ys ++ zs =
= [] ++ (ys ++ zs)

Para ns = (x:xs)

(ns ++ ys) ++ zs =
= ((x:xs) ++ ys) ++ zs =
= (([x] ++ xs) ++ ys) ++ zs = Por HI
= ([x] ++ xs) ++ (ys ++ zs) =
= (x:xs) ++ (ys ++ zs) =
= ns ++ (ys ++ zs) 
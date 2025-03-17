data Nat = Zero | Succ Nat

Zero + y = y
Succ x + y = Succ (x+y)

Vamos mostrar que
x+(y+z) = (x+y)+z
(Hipótese de Indução)

Por indução sobre x

Caso base

x = Zero
Zero+(y+z)=(Zero+y)+z

Zero+(y+z) =
= y+z =
= (Zero+y)+z

Para 
x = Succ n
Succ n + (y+z) = (Succ n + y) + z

Succ n + (y + z) =
= Succ (n+(y+z)) = por HI
= Succ ((n+y)+z) =
= Succ (n + y) + z =
= (Succ n + y) + z
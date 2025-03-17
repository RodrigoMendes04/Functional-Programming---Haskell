import Data.Char (isDigit, isLower, isUpper)

forte :: String -> Bool
forte palavra = length palavra >= 8 && or [isUpper c | c <- palavra] && or [isLower c | c <- palavra] && or [isDigit c | c <- palavra]

-- length palavra >= 8: Verifica se o comprimento da palavra é maior ou igual a 8.
-- or [isUpper c | c <- palavra]: Verifica se existe pelo menos uma letra maiúscula na palavra.
-- or [isLower c | c <- palavra]: Verifica se existe pelo menos uma letra minúscula na palavra.
-- or [isDigit c | c <- palavra]: Verifica se existe pelo menos um dígito na palavra.

-- or: O or é uma função pré-definida em Haskell que retorna True se pelo menos um dos elementos em uma lista for True, e False caso contrário.
-- É útil para verificar se pelo menos uma das condições é verdadeira. Por exemplo, or [True, False, False] retorna True.

-- c <- palavra: Esta é uma expressão em compreensão de lista que itera sobre cada caractere c na string palavra. Basicamente, ele percorre cada
-- caractere da string e atribui cada caractere a c para ser usado na expressão dentro da compreensão de lista. Isso permite que você itere sobre os caracteres
-- de uma string e faça verificações em cada caractere.
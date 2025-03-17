classifica :: Float -> Float -> String
classifica peso altura
    | imc < 18.5 = "IMC: " ++ show imc ++ " - baixo peso"
    | imc < 25   = "IMC: " ++ show imc ++ " - peso normal"
    | imc < 30   = "IMC: " ++ show imc ++ " - excesso de peso"
    | otherwise  = "IMC: " ++ show imc ++ " - obesidade"
    where
        imc = peso / (altura * altura)

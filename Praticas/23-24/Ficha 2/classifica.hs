classifica :: Float -> Float -> String
classifica peso altura
  | imc < 18.5 = "underweight"
  | imc < 25   = "normal weight"
  | imc < 30   = "overweight"
  | otherwise  = "obesity"
  where imc = peso / altura^2

classifica2 :: Int -> String
classifica2 nota = if nota <= 9
                        then "reprovado"
                        else if nota <= 12
                                then "suficiente"
                                else if nota <= 15
                                        then "bom"
                                        else if nota <= 18
                                                then "muito bom"
                                                else "muito bom com distinção"
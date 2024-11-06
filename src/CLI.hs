module CLI (main) where

import CuatroEnLinea

main :: IO ()
main =
  let j0 = nuevo
      (j1, r1) = poner Rojo 1 j0
      (j2, r2) = poner Amarillo 1 j1
   in putStrLn (showJuego j2)

showJuego :: Juego -> String
showJuego j =
  let css = casillas j
   in concat (map (\f -> showFila f ++ "\n") css)

showFila :: [Maybe Color] -> String
showFila l = concat (map showColor l)

showColor :: Maybe Color -> String
showColor Nothing = "⚪️"
showColor (Just Amarillo) = "🟡"
showColor (Just Rojo) = "🔴"
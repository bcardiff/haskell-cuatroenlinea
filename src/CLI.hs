module CLI (main) where

import CuatroEnLinea
import Data.Char
import TinyApp.Interactive

main :: IO ()
main = runInteractive cuatroEnLinea

data State = State
  { juego :: Juego,
    jugadas :: [Columna],
    columnaActual :: Int
  }
  deriving (Show)

cuatroEnLinea :: Sandbox State
cuatroEnLinea =
  Sandbox
    { initialize =
        State
          { juego = nuevo,
            jugadas = [],
            columnaActual = 1
          },
      render = \s ->
        show s
          <> "\n\n\n"
          <> "  "
          <> showColumnaActual (columnaActual s) (turno (juego s))
          <> "\n\n"
          <> showJuego (juego s)
          <> "+---------------+",
      update = \(Key key _) s ->
        case key of
          KEsc -> (s, Exit)
          KChar 'q' -> (s, Exit)
          KChar 'n' -> (s {juego = nuevo}, Continue)
          KChar 'u' ->
            if length (jugadas s) > 0
              then
                let jugadasAnteriores =
                      if esNuevo (juego s)
                        then
                          jugadas s
                        else
                          init (jugadas s)
                 in ( ponerMuchos
                        ( State
                            { juego = nuevo,
                              jugadas = [],
                              columnaActual = columnaActual s
                            }
                        )
                        jugadasAnteriores,
                      Continue
                    )
              else
                (s, Continue)
          KLeft -> (s {columnaActual = max (columnaActual s - 1) 1}, Continue)
          KRight -> (s {columnaActual = min (columnaActual s + 1) 7}, Continue)
          KDown -> (poner' s (columnaActual s), Continue)
          KEnter -> (poner' s (columnaActual s), Continue)
          KChar c ->
            if c >= '1' && c <= '7'
              then
                let numCol = ord c - ord '0'
                 in (poner' s numCol, Continue)
              else
                (s, Continue)
          _ -> (s, Continue)
    }

ponerMuchos :: State -> [Columna] -> State
ponerMuchos s [] = s
ponerMuchos s (c : cs) = ponerMuchos (poner' s c) cs

poner' :: State -> Columna -> State
poner' s numCol =
  let j' = poner numCol (juego s)
   in s
        { juego = fst j',
          jugadas =
            case snd j' of
              Ok -> jugadas s ++ [numCol]
              ColumnaInvalida -> jugadas s
              ColumnaCompleta -> jugadas s
        }

-- jugar :: Juego -> IO ()
-- jugar j = do
--   putStrLn $ showJuego j
--   col <- intPrompt "Donde va la pieza 1-7?"
--   let (j', r') = poner (turno j) col j
--   jugar j'

-- intPrompt :: String -> IO Int
-- intPrompt prompt = do
--   putStrLn prompt
--   input <- getLine
--   pure (read input)

-- main =
--   let j0 = nuevo
--       (j1, r1) = poner Rojo 1 j0
--       (j2, r2) = poner Amarillo 1 j1
--    in putStrLn (showJuego j2)

showJuego :: Juego -> String
showJuego j =
  let css = casillas j
   in concat (map (\f -> "| " <> showFila f <> "|\n") css)

showFila :: [Maybe Color] -> String
showFila l = concat (map showColor l)

-- https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences

showColor :: Maybe Color -> String
showColor Nothing = "  "
showColor (Just Amarillo) = ansiFgYellow <> "● " <> ansiResetColor
showColor (Just Rojo) = ansiFgRed <> "● " <> ansiResetColor

ansiFgRed, ansiFgYellow, ansiResetColor :: String
ansiResetColor = "\ESC[39m\ESC[49m"
ansiFgYellow = "\ESC[31m"
ansiFgRed = "\ESC[33m"

showColumnaActual :: Int -> Color -> String
showColumnaActual i c =
  concat (replicate (i - 1) (showColor Nothing))
    <> showColor (Just c)
    <> concat (replicate (7 - i) (showColor Nothing))

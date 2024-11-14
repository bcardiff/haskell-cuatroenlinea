{-# LANGUAGE DeriveGeneric #-}

module CLI (main) where

import CuatroEnLinea
import Data.Aeson
import Data.Char
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.IO.Error
import TinyApp.Interactive

data Archivo = Archivo
  { jugadasAr :: [Columna],
    columnaActualAr :: Int
  }
  deriving (Generic)

instance ToJSON Archivo

instance FromJSON Archivo

main :: IO ()
main = do
  args <- getArgs
  let nombreArchivo =
        if length args > 0
          then
            head args
          else
            "juego.json"
  dinicial <- catchIOError (decodeFileStrict nombreArchivo) (\_ -> pure Nothing)
  s <- runInteractive' (cuatroEnLinea (cargarArchivo dinicial))
  let dfinal = Archivo {jugadasAr = jugadas s, columnaActualAr = columnaActual s}
  encodeFile nombreArchivo dfinal

  pure ()

cargarArchivo :: Maybe Archivo -> State
cargarArchivo (Just archivo) =
  ponerMuchos
    ( State
        { juego = nuevo,
          jugadas = [],
          columnaActual = columnaActualAr archivo
        }
    )
    (jugadasAr archivo)
cargarArchivo Nothing =
  State
    { juego = nuevo,
      jugadas = [],
      columnaActual = 1
    }

data State = State
  { juego :: Juego,
    jugadas :: [Columna],
    columnaActual :: Int
  }
  deriving (Show)

cuatroEnLinea :: State -> Sandbox State
cuatroEnLinea firstState =
  Sandbox
    { initialize = firstState,
      --   State
      --     { juego = nuevo,
      --       jugadas = [],
      --       columnaActual = 1
      --     },
      render = \s ->
        show s
          <> "\n\n\n"
          <> "  "
          <> showColumnaActual (columnaActual s) (turno (juego s))
          <> "\n\n"
          <> showJuego (juego s)
          <> "+---------------+"
          <> "\n\n"
          <> mostarEstadoFinal (juego s),
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

mostarEstadoFinal :: Juego -> String
mostarEstadoFinal j
  | not (termino j) = ""
  | otherwise =
      case ganador j of
        Nothing -> "Empate!"
        Just c -> "Gano el " <> show c

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
              JuegoTerminado -> jugadas s
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
ansiFgYellow = "\ESC[33m"
ansiFgRed = "\ESC[31m"

showColumnaActual :: Int -> Color -> String
showColumnaActual i c =
  concat (replicate (i - 1) (showColor Nothing))
    <> showColor (Just c)
    <> concat (replicate (7 - i) (showColor Nothing))

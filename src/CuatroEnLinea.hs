module CuatroEnLinea
  ( Juego,
    Color (..),
    Columna,
    Resultado (..),
    nuevo,
    turno,
    poner,
  )
where

type Grilla = ([Color], [Color], [Color], [Color], [Color], [Color], [Color])

data Juego = Juego
  { -- Cada [Color] es una columna de la grilla
    -- la cabeza de la lista es la ficha superior
    grilla :: Grilla,
    actual :: Color
  }
  deriving (Show)

-- >>> poner Amarillo 1 (fst (poner Rojo 1 nuevo))
-- (Juego {grilla = ([Amarillo,Rojo],[],[],[],[],[],[]), actual = Rojo},Ok)

data Color = Amarillo | Rojo
  deriving (Show, Eq)

type Columna = Int

data Resultado
  = Ok
  | TurnoInvalido
  | ColumnaInvalida
  | ColumnaCompleta
  deriving (Show, Eq)

nuevo :: Juego
nuevo =
  Juego
    { grilla = ([], [], [], [], [], [], []),
      actual = Rojo
    }

turno :: Juego -> Color
turno j = actual j

poner :: Color -> Columna -> Juego -> (Juego, Resultado)
poner c col j
  | c /= turno j = (j, TurnoInvalido)
  | col < 1 || col > 7 = (j, ColumnaInvalida)
  | length (dameColumna col j) >= 6 = (j, ColumnaCompleta)
  | otherwise =
      ( Juego
          { grilla = cambiarColumna col (c : dameColumna col j) j,
            actual = if c == Rojo then Amarillo else Rojo
          },
        Ok
      )

dameColumna :: Columna -> Juego -> [Color]
dameColumna col j =
  case (col, grilla j) of
    (1, (g, _, _, _, _, _, _)) -> g
    (2, (_, g, _, _, _, _, _)) -> g
    (3, (_, _, g, _, _, _, _)) -> g
    (4, (_, _, _, g, _, _, _)) -> g
    (5, (_, _, _, _, g, _, _)) -> g
    (6, (_, _, _, _, _, g, _)) -> g
    (7, (_, _, _, _, _, _, g)) -> g

cambiarColumna :: Columna -> [Color] -> Juego -> Grilla
cambiarColumna col nueva j =
  case (col, grilla j) of
    (1, (_, b, c, d, e, f, g)) -> (nueva, b, c, d, e, f, g)
    (2, (a, _, c, d, e, f, g)) -> (a, nueva, c, d, e, f, g)
    (3, (a, b, _, d, e, f, g)) -> (a, b, nueva, d, e, f, g)
    (4, (a, b, c, _, e, f, g)) -> (a, b, c, nueva, e, f, g)
    (5, (a, b, c, d, _, f, g)) -> (a, b, c, d, nueva, f, g)
    (6, (a, b, c, d, e, _, g)) -> (a, b, c, d, e, nueva, g)
    (7, (a, b, c, d, e, f, _)) -> (a, b, c, d, e, f, nueva)

-- contenido :: Columna -> Fila -> Juego -> Maybe Color
-- termino :: Juego -> Bool
-- ganador :: Juego -> Maybe Color

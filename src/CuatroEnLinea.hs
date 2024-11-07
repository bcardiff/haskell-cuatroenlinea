module CuatroEnLinea
  ( Juego,
    Color (..),
    Columna,
    Resultado (..),
    nuevo,
    turno,
    poner,
    casillas,
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

-- >>> casillas $ fst (poner Amarillo 1 (fst (poner Rojo 1 nuevo)))
-- [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Just Amarillo,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Just Rojo,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]

data Color = Amarillo | Rojo
  deriving (Show, Eq)

type Columna = Int

data Resultado
  = Ok
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

alturaColumna :: Int
alturaColumna = 6

poner :: Columna -> Juego -> (Juego, Resultado)
poner col j
  | col < 1 || col > 7 = (j, ColumnaInvalida)
  | length (dameColumna col j) >= alturaColumna = (j, ColumnaCompleta)
  | otherwise =
      ( Juego
          { grilla = cambiarColumna col (c : dameColumna col j) j,
            actual = if c == Rojo then Amarillo else Rojo
          },
        Ok
      )
  where
    c = actual j

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

-- | Devuelve la grilla en formato de filas
casillas :: Juego -> [[Maybe Color]]
casillas j =
  let casillasPorColumna = map (\c -> dameColumnaCompleta c j) [1 .. 7]
   in traspuesta casillasPorColumna

traspuesta :: [[a]] -> [[a]]
traspuesta l
  | length (head l) == 1 = [map head l]
  | otherwise = map head l : traspuesta (map tail l)

-- >>> traspuesta [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]

-- >>> map (\c -> c + 10) [1..7]
-- [11,12,13,14,15,16,17]

-- [f 1,f 2,f 3,f 4,f 5,f 6,f 7]

-- | Devuelve una columna con todas casillas. Las vacian como Nothing
dameColumnaCompleta :: Columna -> Juego -> [Maybe Color]
dameColumnaCompleta c j =
  let justify :: [Color] -> [Maybe Color]
      justify [] = []
      justify (x : xs) = Just x : justify xs

      completarColumna :: [Maybe Color] -> [Maybe Color]
      completarColumna l
        | length l == alturaColumna = l
        | otherwise = completarColumna (Nothing : l)
   in completarColumna (justify (dameColumna c j))

-- implementación alternativa sin recursión
dameColumnaCompleta' :: Columna -> Juego -> [Maybe Color]
dameColumnaCompleta' c j =
  let columna = dameColumna c j
   in replicate (alturaColumna - length columna) Nothing ++ map Just columna

-- [Rojo, Amarillo]

-- [Nothing, Nothing, Nothing, Nothing, Just Rojo, Just Amarillo]

-- contenido :: Columna -> Fila -> Juego -> Maybe Color
-- termino :: Juego -> Bool
-- ganador :: Juego -> Maybe Color

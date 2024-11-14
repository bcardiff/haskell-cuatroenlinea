module CuatroEnLinea
  ( Juego,
    Color (..),
    Columna,
    Resultado (..),
    nuevo,
    esNuevo,
    turno,
    poner,
    casillas,
    termino,
    ganador,
  )
where

type Grilla = ([Color], [Color], [Color], [Color], [Color], [Color], [Color])

data Estado = EnProgreso | Gano Color | Empate
  deriving (Show, Eq)

data Juego = Juego
  { -- Cada [Color] es una columna de la grilla
    -- la cabeza de la lista es la ficha superior
    grilla :: Grilla,
    actual :: Color,
    estado :: Estado
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
  | JuegoTerminado
  deriving (Show, Eq)

nuevo :: Juego
nuevo =
  Juego
    { grilla = ([], [], [], [], [], [], []),
      actual = Rojo,
      estado = EnProgreso
    }

esNuevo :: Juego -> Bool
esNuevo j = grilla j == ([], [], [], [], [], [], [])

turno :: Juego -> Color
turno j = actual j

alturaColumna :: Int
alturaColumna = 6

poner :: Columna -> Juego -> (Juego, Resultado)
poner col j
  | col < 1 || col > 7 = (j, ColumnaInvalida)
  | estado j /= EnProgreso = (j, JuegoTerminado)
  | length (dameColumna col j) >= alturaColumna = (j, ColumnaCompleta)
  | otherwise =
      ( verSiGanoOTerminoColorPoniendo
          col
          c
          Juego
            { grilla = cambiarColumna col (c : dameColumna col j) j,
              actual = if c == Rojo then Amarillo else Rojo,
              estado = EnProgreso
            },
        Ok
      )
  where
    c = actual j

verSiGanoOTerminoColorPoniendo :: Columna -> Color -> Juego -> Juego
verSiGanoOTerminoColorPoniendo col c j
  | hay4 columnaConFichas c = j {estado = Gano c}
  | hay4 (m !! indiceFilaActual) (Just c) = j {estado = Gano c}
  | hay4 diagonalDescendiente (Just c) = j {estado = Gano c}
  | tableroLleno j = j {estado = Empate}
  | otherwise = j
  where
    m = casillas j
    indiceFilaActual = alturaColumna - length columnaConFichas
    columnaConFichas = dameColumna col j
    indiceColumnaActual = col - 1
    dondeEmpiezaDiagonalDesc =
      if indiceFilaActual > indiceColumnaActual
        then
          (indiceFilaActual - indiceColumnaActual, 0)
        else
          (0, indiceColumnaActual - indiceFilaActual)
    diagonalDescendiente = diagonalD m dondeEmpiezaDiagonalDesc

-- (fila, columa)
diagonalD :: [[a]] -> (Int, Int) -> [a]
diagonalD m (f, c)
  | enRango = ((m !! f) !! c) : diagonalD m (f + 1, c + 1)
  | otherwise = []
  where
    enRango = 0 <= f && f < length m && 0 <= c && c < length (m !! f)

tableroLleno :: Juego -> Bool
tableroLleno j =
  all (\col -> columnaLlena (dameColumna col j)) [1 .. 7]

-- tableroLleno j =
--   columnaLlena (dameColumna 1 j)
--     && columnaLlena (dameColumna 2 j)
--     && columnaLlena (dameColumna 3 j)
--     && columnaLlena (dameColumna 4 j)
--     && columnaLlena (dameColumna 5 j)
--     && columnaLlena (dameColumna 6 j)
--     && columnaLlena (dameColumna 7 j)

columnaLlena :: [Color] -> Bool
columnaLlena l = length l == alturaColumna

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

termino :: Juego -> Bool
termino j =
  case estado j of
    EnProgreso -> False
    Gano _ -> True
    Empate -> True

ganador :: Juego -> Maybe Color
ganador j =
  case estado j of
    EnProgreso -> Nothing
    Gano c -> Just c
    Empate -> Nothing

hay4 :: (Eq a) => [a] -> a -> Bool
hay4 l e = length l >= 4 && (take 4 l == [e, e, e, e] || hay4 (tail l) e)

-- hay4 l e =
--   (length l >= 4)
--     && (todosIgualesA (take 4 l) e || hay4 (tail l) e)

-- >>> hay4 [1,2,2,3,2] 2
-- False

todosIgualesA :: (Eq a) => [a] -> a -> Bool
todosIgualesA [] e = True
todosIgualesA (x : xs) e = x == e && todosIgualesA xs e

-- todosIguales' :: (Eq a) => [a] -> Maybe a
-- todosIguales' [] = Nothing
-- todosIguales' (x : []) = Just x
-- todosIguales' (x : xs) =
--   case todosIguales' xs of
--     Nothing -> Nothing
--     Just y -> if y == x then Just x else Nothing

-- >>> todosIguales [Nothing, Nothing]
-- True

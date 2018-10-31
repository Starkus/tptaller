type Conjunto a = [a]
type Tablero = [[Integer]]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13, 12, 19, 6], [7, 13, 32, 6], [22, 20, 14, 7], [7, 33,53, 16], [27, 2, 8, 18]]

sopa2 :: Tablero
sopa2 = [[(-20), (-20), (-20)], [0, 10, 20], [(-10), (-10), 0], [10, 20,(-10)]]

sopa3 :: Tablero
sopa3 = [[10,5,15],[-1,7,2],[2,12,3]]

camino1 :: Camino 
camino1 = [(1,1),(1,2),(2,2),(2,3)]

camino2 :: Camino 
camino2 = [(2,1),(2,2),(2,3),(3,3),(4,3)]

camino3 :: Camino 
camino3 = [(1,2),(2,2),(3,2)]

-- Dado la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Dado la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posicion de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)  

-- Determina si una posicion esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t) 

-- Devuelve una lista con los valores del tablero en el camino
numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino tab [] = []
numerosDeCamino tab (x:xs) = valor tab x : numerosDeCamino tab xs

_pertenece :: Integer -> [Integer] -> Bool
_pertenece n [] = False
_pertenece n (x:xs) = n == x || _pertenece n xs

_hayRepetidos :: [Integer] -> Bool
_hayRepetidos [] = False
_hayRepetidos (x:xs) = _pertenece x xs || _hayRepetidos xs

caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos tablero camino = _hayRepetidos (numerosDeCamino tablero camino) == False


-- Calcula el Maximo del Tablero
-- _maxLista busca el maximo de una lista
_maxLista::[Integer]->Integer
_maxLista x | length x == 1 = head x
            | length x > 1 && head x >= _maxLista (tail x) = head x
            | otherwise = _maxLista (tail x)

-- _maxFilas crea una lista de valores maximos de un Tablero
_maxFilas::Tablero->[Integer]
_maxFilas tab | length tab == 1 = [_maxLista (head tab)]
              | otherwise = _maxLista (head tab) : _maxFilas (tail tab)

-- maximo devuelve el valor maximo del Tablero
maximo::Tablero->Integer
maximo tab | length tab == 1 = _maxLista (head tab)
           | otherwise = _maxLista (_maxFilas tab)


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

-- Camino4 creado con fines de testing
camino4 :: Camino
camino4 = [(2,1),(2,2),(3,2),(4,2),(4,3)]

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

-- Devuelve True si al menos un elemento esta 2 o mas veces
_hayRepetidos :: [Integer] -> Bool
_hayRepetidos [] = False
_hayRepetidos (x:xs) = elem x xs || _hayRepetidos xs

-- Determina si los valores de un camino contiene repetidos
caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos tablero camino = _hayRepetidos (numerosDeCamino tablero camino) == False


-- Calcula el Maximo del Tablero
-- _maxLista busca el maximo de una lista
_maxLista:: [Integer] -> Integer
_maxLista x | length x == 1 = head x
            | length x > 1 && head x >= _maxLista (tail x) = head x
            | otherwise = _maxLista (tail x)

-- _maxFilas crea una lista de valores maximos de un Tablero
_maxFilas:: Tablero -> [Integer]
_maxFilas tab | length tab == 1 = [_maxLista (head tab)]
              | otherwise = _maxLista (head tab) : _maxFilas (tail tab)

-- maximo devuelve el valor maximo del Tablero
maximo:: Tablero -> Integer
maximo tab | length tab == 1 = _maxLista (head tab)
           | otherwise = _maxLista (_maxFilas tab)


-- masRepetido
-- Primero cambio los valores de una fila a tuplas

_valoresTableroRepetidos:: Tablero -> [Integer]
_valoresTableroRepetidos tab | length tab == 1 = head tab
                             | otherwise = head tab ++ _valoresTableroRepetidos (tail tab)
                     
_eliminarRepetidos:: [Integer] -> [Integer]
_eliminarRepetidos [] = []
_eliminarRepetidos (x:xs) | elem x xs = _eliminarRepetidos (xs)
                          | otherwise = [x] ++ _eliminarRepetidos (xs)

_aparicionesK:: [Integer] -> Integer -> Integer
_aparicionesK l k | length l == 1 && head l == k = 1
                  | length l == 1 && head l /= k = 0
                  | length l >= 1 && head l == k = 1 + _aparicionesK (tail l) k
                  | otherwise = _aparicionesK (tail l) k

_cantAparTupla:: [Integer] -> Integer -> (Integer,Integer)
_cantAparTupla l k = (k, _aparicionesK l k)

_cantApar:: [Integer] -> [Integer] -> [(Integer,Integer)]
_cantApar l apar | length apar == 1 = apariciones
                 | otherwise = apariciones ++ _cantApar l (tail apar)
                  where apariciones = [(head apar, snd (_cantAparTupla l (head apar)))]

_tuplasFinales:: Tablero -> [(Integer,Integer)]
_tuplasFinales tab = _cantApar (_valoresTableroRepetidos tab) (_eliminarRepetidos (_valoresTableroRepetidos tab))

-- CaminoFibonacci, verifica si un camino en un tablero cumple con la condicion de que
-- si un numero (del camino) es suma de los 2 anteriores
caminoDeFibonacci:: Tablero -> Camino -> Bool
caminoDeFibonacci tab cam | length num == 1 = True  -- El largo del camino tiene que ser por lo menos 2,
                          | length num == 2 = True  -- ya que se ven los 2 valores anteriores
                          | length num > 2 && head (reverse num) == (head (tail (reverse num))) + head (tail ( tail (reverse num))) = True
                          | otherwise = False  
                            where num = numerosDeCamino tab cam
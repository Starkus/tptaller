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

-- Esta funcion concatena todas las filas del tablero en una sola
_concatenarTabla :: Tablero -> [Integer]
_concatenarTabla tab | length tab == 1 = head tab
                             | otherwise = head tab ++ _concatenarTabla (tail tab)

-- Dado un numero n y una lista, devuelve una lista sin ningun elemento n en ella
_vaciar :: Integer -> [Integer] -> [Integer]
_vaciar _ [] = []
_vaciar n (m:ms) | n == m = _vaciar n ms
                 | otherwise = m : _vaciar n ms

-- Dado un numero n y una lista, devuelve la cantidad de veces que n esta en la lista
_cantApariciones ::  Integer -> [Integer] -> Integer
_cantApariciones _ [] = 0
_cantApariciones n (m:ms) | n == m = 1 + _cantApariciones n ms
                          | otherwise = _cantApariciones n ms

-- Dado un numero n, c = cantidad de veces que n esta en la lista, y una lista, devuelve el elemento mas repetido de la lista
_compararRepeticiones :: Integer -> Integer -> [Integer] -> Integer
_compararRepeticiones n _ [] = n
_compararRepeticiones n c (m:ms) | c >= cantM = _compararRepeticiones n c (_vaciar m ms)
                                 | otherwise = _compararRepeticiones m cantM (_vaciar m ms)
                                 where cantM = _cantApariciones m (m:ms)

-- Invoca a _compararRepeticiones usando n = el 1er elemento de t (el tablero en el cual se uso _concatenarTabla), la cantidad de veces que n esta en t, y lo compara con el resto de los elementos de t (sin n's)
masRepetido :: Tablero -> Integer
masRepetido tab = _compararRepeticiones n (_cantApariciones n t) (_vaciar n t)
                where t = _concatenarTabla tab
                      n = head t

-- <[############]>

--  -- Esta funcion elimina los repetidos de una funcion (usado para la lista
--  -- generada por la funcion anterior)
--  _eliminarRepetidos:: [Integer] -> [Integer]
--  _eliminarRepetidos [] = []
--  _eliminarRepetidos (x:xs) | elem x xs = _eliminarRepetidos (xs)
--                            | otherwise = [x] ++ _eliminarRepetidos (xs)
--  
--  -- Esta funcion cuenta las apariciones de un numero k en una lista l
--  _aparicionesK:: [Integer] -> Integer -> Integer
--  _aparicionesK l k | length l == 1 && head l == k = 1
--                    | length l == 1 && head l /= k = 0
--                    | length l >= 1 && head l == k = 1 + _aparicionesK (tail l) k
--                    | otherwise = _aparicionesK (tail l) k
--  
--  _cantAparTupla:: [Integer] -> Integer -> (Integer,Integer)
--  _cantAparTupla l k = (k, _aparicionesK l k)
--  
--  -- Cuenta la cantidad de apariciones de una tupla
--  _cantApar:: [Integer] -> [Integer] -> [(Integer,Integer)]
--  _cantApar l apar | length apar == 1 = apariciones
--                   | otherwise = apariciones ++ _cantApar l (tail apar)
--                    where apariciones = [(head apar, snd (_cantAparTupla l (head apar)))]
--  
--  -- Esta funcion genera una lista de tuplas con todos los 
--  -- numeros del tablero y la cantidad de apariciones
--  -- y devuelve una tupla de forma : (Nro en Tablero, Apariciones)
--  _tuplasFinales:: Tablero -> [(Integer,Integer)]
--  _tuplasFinales tab = _cantApar (_concatenarTabla tab) (_eliminarRepetidos (_concatenarTabla tab))
--  
--  -- Calcula la tupla con mas repeticiones. Devuelve esta tupla
--  _maximoTupla:: [(Integer,Integer)] -> (Integer,Integer)
--  _maximoTupla l | length l == 1 = head l
--                 | length l > 1 && snd (head l) >= snd (_maximoTupla (tail l)) = head l
--                 | otherwise = _maximoTupla (tail l)
--  
--  -- Agarra la tupla de la funcion anterior y extrae el primer valor
--  masRepetido:: Tablero -> Integer
--  masRepetido tab = fst (_maximoTupla (_tuplasFinales tab))

-- <[############]>

-- CaminoFibonacci, verifica si un camino en un tablero cumple con la condicion de que si un numero (del camino) es suma de los 2 anteriores
caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci tab cam | length num == 1 = True  -- El largo del camino tiene que ser por lo menos 2,
                          | length num == 2 = True  -- ya que se ven los 2 valores anteriores
                          | length num > 2 && head (reverse num) == (head (tail (reverse num))) + head (tail ( tail (reverse num))) = True
                          | otherwise = False  
                            where num = numerosDeCamino tab cam

-- Dado un tablero, y una posicion, devuelve una lista de todos los caminos de fibonacci de mayor longitud posibles que se originen en dicha posicion. El 3er parametro es auxiliar y consta de una lista con la posicion de origen (ej: [(1:1)])
caminosFibDesde :: Tablero -> Posicion -> Camino -> [Camino]
caminosFibDesde t (y,x) c | existeFibADerecha && existeFibAAbajo = caminosFibDesde t derecha caminoDer ++ caminosFibDesde t abajo caminoAb
                          | existeFibADerecha = caminosFibDesde t derecha caminoDer
                          | existeFibAAbajo = caminosFibDesde t abajo caminoAb 
                          | otherwise = [c]
                          where existeFibADerecha = posValida t derecha && caminoDeFibonacci t caminoDer
                                existeFibAAbajo = posValida t abajo && caminoDeFibonacci t caminoAb
                                derecha = (y,x+1)
                                caminoDer = c ++ [derecha]
                                abajo = (y+1,x)
                                caminoAb = c ++ [abajo]

-- Dado un tablero y una posision p, recorre de izquierda a derecha y de arriba a abajo desde p, y devuelve una lista de caminos de fibonacci que se originen en las celdas recorridas. (Usar p=(1,1) para que recorra todo el tablero)
caminosFib :: Tablero -> Posicion -> [Camino]
caminosFib t (y,x) | posValida t derecha = caminosFibDesde t (y,x) [(y,x)] ++ caminosFib t derecha
                   | posValida t derecha == False && posValida t abajo = caminosFibDesde t (y,x) [(y,x)] ++ caminosFib t (y+1,1)
                   | otherwise = caminosFibDesde t (y,x) [(y,x)]
                   where derecha = (y,x+1)
                         abajo = (y+1,x)

-- Dado una lista de listas, devuleve la lista con mayor elementos
mayorLista :: [[a]] -> [a]
mayorLista [c] = c
mayorLista (c1:c2:cs) | length c1 >= length c2 = mayorLista (c1:cs)
                      | otherwise = mayorLista (c2:cs)

-- Si
mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci t = numerosDeCamino t (mayorLista (caminosFib t (1,1)))

-- Dado una lista de listas y un nro n, devuelve una lista de listas de longitud n. Las listas de mayor longitud a n se les quita los ultimos elementos asi tienen longitud n
filtrarListasLongitud :: [[a]] -> Integer -> [[a]]                 --(Int porque lenght devuelve valores de tipo Int)
filtrarListasLongitud [] _ = []
filtrarListasLongitud (l:ls) n | length l == val = l : filtrarListasLongitud ls n
                               | length l > val = (quitarUltimos l ((length l) - val)) : filtrarListasLongitud ls n
                               | otherwise = filtrarListasLongitud ls n
                                where val = fromIntegral n

-- Dada una lista y un nro n, devuelve una lista sin los ultimos n elementos
quitarUltimos :: [a] -> Int -> [a]
quitarUltimos (l:ls) n | length (l:ls) == n = []
                       | otherwise = l : quitarUltimos ls n

-- Dado una lista l y una lista de listas, devuelve una lista de listas sin ninguna lista l
sacarElemento :: [Integer] -> [[Integer]] -> [[Integer]]
sacarElemento _ [] = []
sacarElemento l (j:js) | l == j = sacarElemento l js
                       | otherwise = j : sacarElemento l js

-- Dado una lista de listas, devuelve un conjunto de listas (sin elementos repetidos)
conjuntoDeListas :: [[Integer]] -> Conjunto[Integer]
conjuntoDeListas [] = []
conjuntoDeListas (l:ls) = l : conjuntoDeListas(sacarElemento l ls)

valoresDeCaminos :: Tablero -> [Camino] -> [[Integer]]
valoresDeCaminos t [] = []
valoresDeCaminos t (c:cs) = numerosDeCamino t c : valoresDeCaminos t cs

-- Tambien
secuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto [Integer]
secuenciasDeFibonacciDeLongitudK t k = conjuntoDeListas (valoresDeCaminos t (filtrarListasLongitud (caminosFib t (1,1)) k))
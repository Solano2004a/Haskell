{-
digs2Int : Convierte una lista de digitos en un numero entero.
digs2Int [2,3,4,5]
Resultado: 2345
-}


{-
sumLL :: Dada una lista de listas suma todos los valores de las sublistas
sumLL [[1,3],[2,5]]
Resultado: 11
-}

{-
borra : elimina todas las ocurrecias de un elemento dentro de una lista
borra 5 [2,3,5,6]
Resultado: [2,3,6]
borra 5 [2,3,5,6,5]
Resultado:  [2,3,6]
borra 7 [2,3,5,6,5]
Resultado: [2,3,5,6,5]
-}

{-
pertenece: Verifica si un elemento pertenece o no a una lista
pertenece [3,5,1,7] 2
Resultado: False
pertenece [3,5,1,7] 7
Resultado: True
-}


{-
Definir la funcion Prefijos usando foldr
prefijos [1,2,3]
Resultado: [[],[1],[1,2],[1,2,3]]
-}

--1
digs2Int :: [Int] -> Int
digs2Int = foldl (\acc x -> acc * 10 + x) 0
--2
sumLL :: [[Int]] -> Int
sumLL = sum . map sum
--3
borra :: Eq a => a -> [a] -> [a]
borra x = filter (/= x)
--4
pertenece :: Eq a => a -> [a] -> Bool
pertenece x = any (== x)
--5
prefijos :: [a] -> [[a]]
prefijos = foldr (\x acc -> [] : map (x:) acc) [[]]

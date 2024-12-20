{-
digs2Int : Convierte una lista de digitos en un numero entero.
digs2Int [2,3,4,5]
Resultado: 2345
-}

digs2Int :: [Int] -> Int
digs2Int = foldl (\acc x -> acc *10 +x) 0


--digs2Int [] = ""
--digs2Int (x:xs) = show x ++ digs2Int xs


{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v []     = v
foldr f v (x:xs) = x `f` foldr f v xs -- Infijo
	   
foldl :: (b -> a -> b) -> b -> [a] -> b	   
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs

-}
{-
sumLL :: Dada una lista de listas suma todos los valores de las sublistas
sumLL [[1,3],[2,5]]
Resultado: 11
-}
--sum :: [Int] -> Int
--sum [] = 0
--sum = foldl (+) 0

sumLL::[[Int]]->Int
sumLL [] = 0
sumLL (x:xs) = sumL x + sumLL xs 
  where
    sumL ::[Int] -> Int
    sumL xs = foldl (+) 0 xs

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
borra :: Int -> [Int] -> [Int]
borra _ [] = []
borra a (x:xs)  | x == a    = borra a xs
                | otherwise = x: borra a xs
-}

borra :: Eq a => a -> [a] -> [a]
borra y = foldr (\x acc -> if y == x then acc else x:acc ) []

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
{-

1. digs2Int :: [Int] -> Int
digs2Int = foldl (\acc x -> acc * 10 + x) 0

2. sumLL :: [[Int]] -> Int
sumLL = sum . map sum

3. borra :: Eq a => a -> [a] -> [a]
borra x = filter (/= x)

4. pertenece :: Eq a => a -> [a] -> Bool
pertenece x = any (== x)

5. prefijos :: [a] -> [[a]]
prefijos = foldr (\x acc -> [] : map (x:) acc) [[]]

-}
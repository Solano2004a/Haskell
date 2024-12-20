--MAP Y FILTER
{-
Definir una función squareall :: [Int] → [Int] que toma una lista de enteros y
produce una lista de los cuadrados de esos enteros. Por ejemplo, 
squareall [6, 1, (-3)] = [36, 1, 9]. 

-}
cuadrados :: [Int] -> [Int]
cuadrados = map (^2)

{-Define una función atfront :: a → [[a]] → [[a]] que toma un objeto y una lista de
listas y coloca el objeto al principio de cada lista componente. Por ejemplo,
atfront 7 [[1,2], [], [3]] = [[7,1,2], [7], [7,3]].
-}

atfront :: a -> [[a]] -> [[a]]
atfront x = map (x:)

{-Define una función longitudes que tome como argumento una lista de cadenas y
devuelva la lista de sus longitudes. Por ejemplo, longitudes [«el», «fin», «es»,
«cerca»] = [3, 3, 2, 4]. 
-}

longitudes :: [String]-> [Int]
longitudes = map length

--4:
sumaC :: Int -> Int
sumaC x = suma x
suma:: Int-> Int
suma 0 = 0
suma n = n^2 + suma (n-1)

sumaCmap :: Int -> Int
sumaCmap n = sum (map (^2) [1..n])

--5:
wc :: [a]->[a]
wc = filter (not . ( `Elem` [A..Z]))

--6:
sp :: [Int] -> [Int]
sp = filter (not . esPrimo)
esPrimo :: Int -> Bool
esPrimo n | x >= 2    = [x | x <- [2..(n-1)], n `mod` x ==0]
          | otherwise = False                           

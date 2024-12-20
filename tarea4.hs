-- Definir usando Recursion y Comprensión de listas:

{- 1.
Defina una funcion anadir que añada un elelmentos a cada lista de una lista de listas.

>anadir 4 [[3,5,7],[5,7],[]]
[[4,3,5,7],[4,5,7],[4]]

>anadir 'a' ["hora","batir","sentir"]
["ahora","abatir","asentir"]
-}
--comprension
anadir :: a -> [[a]] -> [[a]]
anadir a [] = [[a]]
anadir a xs = [a:x | x<-xs]
--anadir a xs = map(a:) xs

--Recursion
anadirRecursivo :: a -> [[a]] -> [[a]]
anadirRecursivo _ [] = []
anadirRecursivo a (x:xs) = (a:x) : anadirRecursivo a xs

{- 2.
Escribir una funcion memberNum x ls que devuelve el numero de veces que x aparece en ls.

Usa memberNum para escribir una funcion unico ls que devuelve una lista de elementos de ls que aparecen solo una vez.

> memberNum 5 [1,5,2,3,5,5]
3
-}
--comprension
memberNum :: Int-> [Int] -> Int
memberNum a xs = sum [1 | x<- xs, x == a]

--memberNumRecursivo
memberNumRecursivo :: Int-> [Int] -> Int
memberNumRecursivo _ [] = 0
memberNumRecursivo a (x:xs)  
    | a == x    = 1 + memberNumRecursivo x xs
    | otherwise = memberNumRecursivo x xs


{- 3.
Usa memberNum para escribir una funcion unico ls que devuelve una lista de elementos de ls que aparecen solo una vez.

> unico [2,4,2,1,4]
[1]

-}
--comprension
unico :: [Int]->[Int]
unico [] = []
unico xs = [x | x<-xs, memberNum x xs == 1]

--recursivo 
{- unicoRecursivo :: [Int]->[Int]
unicoRecursivo [] = []
unicoRecursivo (x:xs) 
  | memberNumRecursivo x (x:xs) == 1 = x: unicoRecursivo xs
  | otherwise               = unicoRecursivo xs
-}
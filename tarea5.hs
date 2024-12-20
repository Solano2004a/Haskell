{-# OPTIONS_GHC -fno-warn-tabs #-}
-- RECURSION

{-
Definir una funcion maximo que calcule el maximo valor de una lista de numeros positivos.
Esta funcion es parcial.

maximo [4,6,1,2,9,5,3,8]
9

-}

maximo ::[Int]->Int
maximo [] = 0
maximo [x] = x
maximo (x:y:xs) | y>=x      = maximo (y:xs)
                | otherwise = maxim0 (x:xs)


{-
Define la función ocurrencias que toma una lista y un dato y devuelve el número de veces que aparece el dato en la lista:

> ocurrencias 2 [1, 2, 3, 2, 4, 5, 2]
3
> ocurrencias 'a' "la casa roja"
4
-}
ocurrencias ::Eq a => a -> [a] -> Int
ocurrencias _ [] = 0
ocurrencias a (x:xs)  | a == x    = 1 + ocurrencias a xs
                      | otherwise = ocurrencias a xs

{-
Definir una funcion recursiva subList que dada una lista, una posicion inicial p y un numero de caracteres n devuelve la sublista que comienza en p y tiene n caracteres

>subList [1,2,3,4,5,6,7] 3 3
[3,4,5]
>subList "Juan Perez" 6 5
"Perez"
-}

sublist :: [a]-> Int -> Int -> [a]
sublist [] _ _ = []
--sublist [] _ 0 = []
sublist (x:xs) p n  | p > 0     = sublist xs (p-1) n
                    | otherwise = case n of 
                      0 -> []
                      _ -> x:sublist xs 0 (n-1) 


-- COMPRENSION DE LISTAS

{- Defina una funcion posiciones que devuelva una lista de índices de las posiciones de un elemento determinado en una lista de elementos. Por ejemplo:

posiciones 4 [1,4,3,7,4,2] devuelve [2,5]
posiciones [3,5] [[3,6],[2,5]] devuelve []
-}

posiciones:: Eq a => a -> [a] -> [Int]
posiciones _ [] = []
posiciones a xs = [i | (i,x) <- zip [1..] xs, x ==a]


{-
Definir una funcion recursiva subList que dada una lista, una posicion inicial p y un numero de caracteres n devuelve la sublista que comienza en p y tiene n caracteres

>subList [1,2,3,4,5,6,7] 3 3
[3,4,5]
>subList "Juan Perez" 6 5
"Perez"
-}

sublista :: [a]->Int->Int->[a]
sublista [] _ _ = []
sublista xs p n = [x | (i,x) <- zip [1..] xs, i >= p && i < (p + n) ]


-- RECURSION CON ACUMULADOR

{-
Defina un funcion sonPares que dada una lista de enteros devuelve una lista de valores booleanos que indican si son pares o no.

sonPares [2,4,5,5,2,1,6,8]
[True,True,False,False,True,False,True,True]
-}
sonParesFunc ::[Int] -> [Bool]
sonParesFunc xs = sonPares xs []
sonPares :: [Int]->[Bool]->[Bool]
sonPares [] acc = reverse acc
sonPares (x:xs) acc | x `mod` 2 == 0 = sonPares xs (True:acc)
                    | otherwise      = sonPares xs (False:acc)
{-
Definir una funcion producto de los elementos de una lista numérica

product [1,2,3]
6
-}
producto ::[Int]->Int
producto xs = producto' 1 xs where
  producto' ::Int->[Int]->Int
  producto' acc [] = acc
  producto' acc (x:xs) = producto' (acc*x) xs


-- DEFINIR USANDO LA TECNICA DE CAPTURA DE RECURSION POR PATRONES

inits ::[Int] -> [[Int]]
inits [] = [[]]
inits (x:xs) = []:[(x:rs) | rs <- inits xs]

{-
Definir una funcion mmi (memores, mayores, iguales) que dados un numero y una lista devuelve tres listas conteniendo los menores, mayores e iguales al primer argumento.

mmi 7 [1,4,2,7,8,4,9,7,6,5]
([1,4,2,4,6,5],[8,9],[7,7])
-}

mmi :: Int -> [Int] -> [[Int]]
mmi _ [] = [[], [], []]
mmi a (x:xs)  | x < a     = [x:menores, mayores, iguales]
              | x > a     = [menores, x: mayores, iguales]
              | otherwise = [menores, mayores, x: iguales]
              where [menores, mayores, iguales] = mmi a xs


{-
Definir una funcion cpn (cantidad de positivos negativos) que cuente la cantidad de numeros positivos y negativos en una lista.

cpn [1,4,-2,7,9,-6,-3,9]
(5,3)
-}
cpn :: [Int]-> (Int,Int)
cpn [] = (0, 0)
cpn (x:xs)    | x > 0     = (1 + positivos, negativos)
              | x < 0     = (positivos, 1 + negativos)
              | otherwise = (positivos, negativos)
              where (positivos, negativos) = cpn xs

{-
Dar ejemplos de valores o definiciones de los siguientes tipos:
-}

bools :: [Bool]
bools = undefined
nums  :: [[Int]]
nums = undefined
add   :: Int -> Int -> Int -> Int
add = undefined
copy  :: a -> (a,a)
copy = undefined
apply :: (a -> b) -> a -> b
apply = undefined

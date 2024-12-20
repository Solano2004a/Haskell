-- Crear las siguientes funciones siguiendo la rreceta de Hutton

{-
elem :: Devuelve si un elemento esta presente o no en la lista
> 1 `elem` [1,2]
True
> 3 `elem` [1,2]
False
> 2 `elem` []
False
-}

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = (x == y) || elem x ys


{-
take : Toma n elementos de una lista.
> take 2 [1,2,3]
[1,2]
> take 0 [1,2,3]
[]
> take 4 [1,2,3]
[1,2,3]
-}
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs



{-
\\		Diferencia de listas
> [1,2] \\ [1]
[2]
> [1,2] \\ [2,3,4]
[1]
> [] \\ [1,2,3]
[]
-}

(diferencia) :: Eq a => [a] -> [a] -> [a]
[] diferencia _ = []
xs diferencia [] = xs
(x:xs) diferencia ys
    | x `elem` ys = xs diferencia ys
    | otherwise   = x : (xs diferencia ys)


{-
init:	Devuelve la lista sin el ultimo elemento.
> init [1,2,3]
[1,2]
> init []
*** Exception: Prelude.init: empty list
-}

init:: [Int]->[Int]
init [] = error "Prelude.init: empty list"
init [_] = []
init (x:xs) = x: init xs

--comprension
init xs = [x | (x,y)<- zip xs [1..lenght xs -1]]







init :: [a] -> [a]
init [] = error "Prelude.init: empty list"
init [_] = []
init (x:xs) = x : init xs



{-
zip xs ys convierte dos listas en una lista de tuplas.
> zip [1,2] [3,4]
[(1,3),(2,4)]
> zip [1,2] [3,4,5]
[(1,3),(2,4)]
-}
zip :: [a]->[b]->[(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = [(x,y) : zip xs ys]











zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

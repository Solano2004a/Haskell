--arreglo de n's, se le pasa la n de la coordenada, que por defecto siempre sera 8 si se encuentra en diagonal de tablero
rangeDiagonalLeft::Int -> Int->[Int] --valor de la columna c y la suma de coordenadas (x+y). Devuelve todas las posibles de n
rangeDiagonalLeft c n | c <= 4    = [n-(2*c),(n-(2*c)) + 2 ..n] ++ [n+2,n+4..n+(2*c)]
                  | otherwise = [n-(2*c'),(n-(2*c')) + 2 ..n] ++ [n+2,n+4..n+(2*c')] where c' = 8-c --se vuelca llegada la mitad del tablero


--devuelve todas las diagonales del tablero
diagonals::[[a]]->[[a]]
diagonals []       = repeat []
diagonals (xs:xss) = takeWhile (not . null) $
    zipWith (++) (map (:[]) xs ++ repeat [])
                 ([]:diagonals xss)
--me devuelve la diagonal derecha del tablero
diagonalRight::Matrix a -> [a]
diagonalRight diags = maximumBy (comparing length) (diagonalsRight diags)

--Verifica si un valor esta en la diagonal derecha o izquierda del tablero
isInDiagonal::Grid->(Int,Int)->Bool
isInDiagonal grid (r,c) = isInDiagonalLeft grid (r,c) || isInDiagonalRight grid (r,c)
  where
    n = length grid --9
    isInDiagonalLeft grid (x,y) = x==y 
    isInDiagonalRight grid (x,y) = y+x == n-1 


--me muestra todas las soluciones de mySudoku_diagonales
search::Matrix Choices -> [Grid]
search m  | not (safe m) || any (any null) m = []
          | complete m = collapse m --solucion(es)
          | otherwise =  [g |m' <- expand m, g <- search (prune m')]


--ENCONTRAR '-'
--genera las posiciones de las celdas vacias
--findEmptyCells::Grid->[(Int,Int)]
--findEmptyCells grid = [(r,c)| (r,row) <- zip [0..] grid, (c,cell) <- zip [0..] row, cell == '-']

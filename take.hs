{-
  > take 2 [1,2,3]
  [1,2]
  > take 0 [1,2,3]
  []
  > take 4 [1,2,3]
  [1,2,3]
-}
funcion :: Int -> [Int] -> [Int]
funcion 0 xs = []
funcion _ [] = []
funcion a (x:xs) = x : funcion (a-1) xs

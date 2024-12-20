--insertar :: Ord a=> Maybe a -> [a] -> [a]
insertar a [] = [a]
--insertar Nothing xs = xs
insertar a (x:xs) | a <= x = a:x:xs
                         | otherwise = x: insertar a xs
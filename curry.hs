addC :: Int -> (Int->Int)
addC x y = x+y

--curry
distancia :: Float -> Float -> Float
distancia x y = sqrt(x^2+y^2)
{-
uncurry1' :: (a->b->c) -> ((a,b)->c)
uncurry1' f (x,y) = f x y

distancias1 :: [(Float,Float)] -> [Float]
distancias1 = map (uncurry' distancia)
-}





uncurry' :: (a->b->c) -> ((a,b) -> c)
uncurry' fc (x,y) = fc x y 

distancias :: [(Float,Float)] -> [Float]
distancias = map (uncurry' distancia)
--distancias xs = map (\(x,y) -> distancia x y) xs
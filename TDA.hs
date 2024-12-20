data Point = Pt Float Float deriving (Show)

data Shape =  Rectangle Point Float Float
            | Circle Point Float
            | Triangle Point Point Point

--perimeter :: Shape -> Float

area :: Shape -> Float
area (Rectangle _ w h) = w * h
area (Circle _ r) = 3.14*r*r
area (Triangle x y z) = sqrt (s * (s-a)*(s-b)*(s-c))
  where a = distance x y
        b = distance y z
        c = distance x z
        s = (a+b+c) /2
        distance (Pt u1 u2) (Pt v1 v2) = sqrt((u1-u2)^2+(v1-v2)^2)

norm :: Point -> Float
norm (Pt x y) = sqrt (x*x + y*y)

zipPoint :: [Float] -> [Float] -> [Point]
zipPoint xs ys = map (uncurry Pt) (zip xs ys)


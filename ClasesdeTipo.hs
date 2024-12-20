data Point = Pt Float Float

instance Eq Point where
  Pt x y == Pt u v = x == u && y == v



{-
--class Eq a where
--(==) :: a -> a -> Bool
--(=/) :: a -> a -> Bool

data Point = Pt Float Float 
  deriving (Show)

instance Eq Point where
  Pt x y == Pt u v = x == u && y == v 
  Pt x y /= Pt u v = u /= x || v /= y

-}

--Instancias en tuplas y listas -> Igualdad/comparacion
{-
instance Eq (a, b) where
  (x, y) == (u, v) = x == u && y==v 

-}





--Instancia Recursiva para Eq en Tree
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a) deriving (Show)


myTree1 :: Tree Int
myTree1 = Node 1 
            EmptyTree
            (Node 2 (Node 3 EmptyTree EmptyTree)
                    EmptyTree)
myTree2 :: Tree Int
myTree2 = Node 1 
            EmptyTree
            (Node 2 (Node 3 EmptyTree EmptyTree)
                    EmptyTree)


instance Eq a => Eq (Tree a) where
  (Node x xl xr) == (Node y yl yr) =  x == y && xl == yl && xr == yr
  EmptyTree == EmptyTree = True
  _ == _                 = False
  --(Node x xl xr) == (Node y yl yr) = x == y && xl == yl && xr == yr
  --Node x (Tree xl) (Tree xr) ==  Node y (Tree yl) (Tree yr) = x == y && 

main :: IO ()
main = do
    print myTree1
    print myTree2
    print (myTree1 == myTree2)


--INSTANCIA DE CLASES
data Vector = Vec Float Float deriving (Show)

data Shape =  Rectangle Point Float Float
  | Circle Point Float 
  | Triangle Point Point Point

deriving instance Show Point
deriving instance Show Shape

class Scalable s where
  scalable :: Float -> s -> s
instance Scalable Shape where
  scalable s (Rectangle p x y) = Rectangle p (s*x) (s*y)



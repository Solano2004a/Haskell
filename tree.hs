--elemList
data IntList =  EmptyList 
              | Cons Int IntList deriving (Show)


elemList :: Int -> IntList -> Bool
elemList x EmptyList = False
elemList x (Cons y ys)  | x == y    = True
                        | otherwise = elemList x ys

--Construccion de una lista
myList :: IntList
myList = Cons 1 (Cons 2 (Cons 3 EmptyList))


--elemTree
data IntTree = EmptyTree
              | Node Int IntTree IntTree deriving (Show)

--ysl -> left Tree 
--ysr -> right Tree
elemTree :: Int -> IntTree -> Bool
elemTree x EmptyTree = False
elemTree x (Node y ysl ysr) | y == x = True
                            | otherwise = elemTree x ysl || elemTree x ysr
                          
myTree1 :: IntTree
myTree1 = Node 1 
            EmptyTree
            (Node 2 (Node 3 EmptyTree EmptyTree)
                    EmptyTree)

myTree2 :: IntTree
myTree2 = Node 9 
            EmptyTree
            (Node 5 (Node 7 EmptyTree EmptyTree)
                    EmptyTree)

--TreeHeight
treeHeight :: IntTree -> Int
treeHeight EmptyTree = 0
treeHeight (Node x ls rs) = 1 + max (treeHeight ls) (treeHeight rs)







--treeToList (Convertir un arbol en una lista)
--(Altura (longitud maxima de la raiz a un arbol vacio) y tamanio(cantidad de nodos que tiene))
{-
data IntList =  EmptyList 
              | Cons Int IntList
data IntTree = EmptyTree
              | Node Int IntTree IntTree
-}
treeToList :: IntTree -> IntList
treeToList EmptyTree = EmptyList
treeToList (Node x ls rs) = Cons  x (concatList ls' rs')
  where
    ls' = treeToList ls
    rs' = treeToList rs

concatList :: IntList -> IntList -> IntList
concatList EmptyList ys = ys
--concatList ls EmptyList = 
concatList (Cons x xs) ys = Cons x (concatList xs ys)
{-
      ---
  ***
--    ---
  ***
      ---

--******
-}





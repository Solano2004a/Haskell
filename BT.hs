-- Practica de Arboles Binarios

--1.- Arboles Binarios

--Ejercicios

-- Definir el tipo de datos árbol binario
data Tree a = EmptyTree
          | Node a (Tree a) (Tree a) deriving (Show)

myTree1 :: Tree Int
myTree1 = Node 4 
            (Node 5 EmptyTree
                    EmptyTree)
            (Node 2 (Node 3 EmptyTree 
                            EmptyTree)
                    EmptyTree)
                    

-- Definir la función tamaño del árbol
tamano:: Tree a -> Int
tamano EmptyTree = 0
tamano (Node a rs ls) = 1 + tamano rs + tamano ls


-- Definir la función profundidad del árbol
profundidad :: Tree a -> Int 
profundidad EmptyTree = 0
profundidad (Node a rs ls) = 1 + max (profundidad rs) (profundidad ls)
-- Definir una función numero de Hojas en el árbol

numHojas :: Tree a -> Int 
numHojas EmptyTree = 0
numHojas (Node a EmptyTree EmptyTree) = 1
numHojas (Node a rs ls) = numHojas rs + numHojas ls
-- Definir la función insertar un nuevo elemento en un árbol
insertar:: Ord a => a -> Tree a -> Tree a
insertar x EmptyTree = Node x EmptyTree EmptyTree
insertar x (Node y rs ls) | x > y     = Node y (insertar x rs) ls
                          | x < y     = Node y rs (insertar x ls)
                          | otherwise = Node y rs ls
-- Crear un árbol a partir de una lista
listToTree::Ord a => [a]->Tree a
listToTree [] = EmptyTree
listToTree (x:xs) =  insertar x (listToTree xs)
--listToTree = foldr insertar EmptyTree

-- Convertir un árbol en lista usando el recorrido en preorden
treeToList :: Tree a -> [a]
treeToList EmptyTree = []
treeToList (Node x rs ls) = [x] ++ treeToList ls ++ treeToList rs


-- Convertir un árbol en lista usando el recorrido en inorden
inorden :: Tree a -> [a]
inorden EmptyTree = []
inorden (Node x rs ls) = treeToList ls ++ [x] ++ treeToList rs


-- Convertir un árbol en lista usando el recorrido en postorden
postorden :: Tree a -> [a]
postorden EmptyTree = []
postorden (Node x rs ls) = postorden ls ++ postorden rs ++ [x]


-- Convertir un árbol en lista usando el recorrido por niveles
{-
myTree1 = Node 4 
            (Node 5 EmptyTree
                    EmptyTree)
            (Node 2 (Node 3 EmptyTree 
                            EmptyTree)
                    EmptyTree)
myTree1 = Node 4 (Node 5 EmptyTree EmptyTree)(Node 2 (Node 3 EmptyTree  EmptyTree) EmptyTree)

        4
      /   \
    2       5
      \
        3
-}
niveles:: Tree a -> [a]
niveles s = aux [s] where
  aux [] = []
  aux (EmptyTree: xs) = aux xs
  aux ((Node x rs ls) : xs) = x : aux (xs ++ [ls, rs])
--niveles (Node x rs ls) = [x] ++ (Node x rs niveles ls) ++ niveles rs



-- Definir una funcion que devuelva los elementos de un nivel dado.
elemNivel :: Int -> Tree a -> [a]
elemNivel _ EmptyTree = []
elemNivel 0 (Node x rs ls) = [x]
elemNivel n (Node x rs ls) = elemNivel (n-1) ls ++ elemNivel (n-1) rs


-- Definir una función que determine si dos árboles binarios son Equivalentes
equivalentes :: Eq a => Tree a -> Tree a -> Bool
equivalentes EmptyTree EmptyTree = True
equivalentes (Node x rx lx) (Node y ry ly) = x == y && equivalentes rx ry && equivalentes lx ly
equivalentes _ _ = False


-- Definir una función que determine si dos árboles binarios son Similares
similares :: Eq a => Tree a -> Tree a -> Bool
similares EmptyTree EmptyTree = True
similares (Node x rx lx) (Node y ry ly) = similares rx ry && similares lx ly
similares _ _ = False

-- Definir una función que determine si un árbol binario es completo.


-- Definir la función map sobre arboles binarios
-- Definir la función foldAB sobre arboles binarios.
-- Definir la suma de los elementos del arbol binario usando foldAB.
-- Definir la función tamaño  usando foldAB

--Construir un arbol balanceado:


{-# OPTIONS_GHC -fno-warn-tabs #-}
-- ===========================================================================
-- Examen Final 2024
-- Programacion Funcional
-- Departamento de Sistemas Computacionales
-- Universidad Privada Boliviana
-- Fecha: 20-diciembre-2023
-- Codigo:  79166


-- ===========================================================================

-- NOTA: Todas las funciones definidas deben tener su tipo especificado

-- 1
--(10 puntos) Definir una funcion que verifique que una expresion formada de parentesis, corchetes y llaves está bien formada

{-
parentesis "(())"
True
parentesis "(()})"
False
parentesis "{[()[]{(())()}]}"
True

-}
-- Verifica si los paréntesis están balanceados usando recursión

parentesis::String->Bool
parentesis s = check s []
  where
    check::String->String->Bool
    check [] [] = True
    check [] _ = False
    check (x:xs) pila 
      | x `elem` "({["  = check xs (x:pila) --pila de entradas
      | x `elem` ")}]"  = not (null pila) && verify (firstElement pila) x && check xs (restElement pila)
      | otherwise       = check xs pila

verify::Char->Char->Bool
verify '(' ')' = True
verify '{' '}' = True
verify '[' ']' = True
verify _ _ = False


firstElement :: [a] -> a
firstElement (x:_) = x  
restElement :: [a] ->[a]
restElement (x:xs) = xs





-- 2
-- (10 puntos) Escriba una funcion

--doUntil :: (Int -> Int) -> (Int -> Bool) -> Int -> Int
--doUntil = undefined

-- tal que (doUntil f p x) devuelva el primer elemento de la serie
--    x; f x; f (f x); f (f (f x)); ...
-- para el que p devuelva True.

{-
Por ejemplo, si twice es la funcion twice x = 2*x,
entonces:

doUntil twice (\y -> y>30) 3 devuelve 48, porque 48 es el primer valor mayor que 30 en la serie:
3; 6; 12; 24; 48

doUntil (+3) (>100) 4 devuelve 103, porque 103 es el primer valor mayor que 100 en la serie:
4; 7; 10; 13; .. ; 97; 100; 103
-}

doUntil :: (Int -> Int) -> (Int -> Bool) -> Int -> Int
doUntil f p x
  | p x       = x
  | otherwise = doUntil f p (f x)



{- 3 (10 puntos)

Diámetro de un árbol binario

El diámetro de un árbol es el número de nodos del camino más largo entre dos hojas del árbol. El diagrama siguiente muestra dos árboles de diámetro nueve cada uno (ver PDF)(note que puede haber más de un camino en el árbol del mismo diámetro).

Entrada :
            1
          /   \
        2      3
      /  \
    4     5

Salida : 4

Entrada:
            1
          /   \
        2      3
      /  \      \
    4     5      6

Salida: 5

-}
-- Estructura de Datos


data Arbol = Nil 
           | Nodo Arbol Int Arbol
           deriving (Show, Eq)





height :: Arbol -> Int
height Nil = 0
height (Nodo left _ right) = 1 + max (height left) (height right)

diametro::Arbol -> Int 
diametro Nil = 0
diametro (Nodo left _ right) = maximum [diamRight, diamLeft, diamRoot]
  where
    diamRoot = height left + height right + 1
    diamRight = diametro right
    diamLeft = diametro left
    

-- Arboles de Ejemplo			
diametro9A  = Nodo (Nodo (Nodo Nil 1 Nil) 1 (Nodo (Nodo Nil 1 Nil) 1 (Nodo Nil 9 Nil))) 
                   1 
				   (Nodo Nil 1 (Nodo Nil 1 (Nodo (Nodo Nil 1 (Nodo Nil 9 Nil)) 
				                                 1 
												 (Nodo Nil 1 Nil)
										   )
							   )
				   )

				   

diametro9B = Nodo (Nodo (Nodo (Nodo Nil 1 Nil) 
                              1 
						      (Nodo (Nodo Nil 1 (Nodo Nil 9 Nil)) 1 Nil)
				        ) 
                        1 
						(Nodo Nil 
						      1 
							  (Nodo (Nodo Nil 1 Nil) 
							        1 
									(Nodo (Nodo Nil 9 Nil) 1 Nil)))
				  ) 
                  1 
                  (Nodo Nil 1 Nil)

		


{-  4 
(10 puntos)
Validar un Arbol Binario de Busqueda

Dado un árbol binario, determine si se trata de un árbol de búsqueda binario (ABB) válido.

Un ABB válido se define como sigue:

	- El subárbol izquierdo de un nodo contiene sólo nodos con valores menores que la clave del nodo.
	- El subárbol derecho de un nodo contiene sólo nodos con valores mayores que la clave del nodo.
	- Tanto el subárbol izquierdo como el derecho deben ser árboles de búsqueda binarios.
-}

-- Arboles de Ejemplo			
esAbb   = Nodo (Nodo Nil 1 Nil) 2 (Nodo Nil 3 Nil)			

noEsAbb = Nodo (Nodo Nil 1 Nil) 5 (Nodo (Nodo Nil 3 Nil) 4 (Nodo Nil 6 Nil))

--validarAbb = undefined



validarAbb :: Arbol -> Bool
validarAbb arbol = esValido arbol Nothing Nothing
  where
    esValido :: Arbol -> Maybe Int -> Maybe Int -> Bool
    esValido Nil _ _ = True
    esValido (Nodo izq valor der) minVal maxVal =
      --valor en posicion correcta
      (maybe True (< valor) minVal) && 
      (maybe True (> valor) maxVal) &&
      --Verificacion de subarboles
      esValido izq minVal (Just valor) &&
      esValido der (Just valor) maxVal



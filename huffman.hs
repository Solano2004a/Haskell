import Data.List 
import Data.Char
--import Test.QuickCheck

mensaje = "Hola Mundo"

-- =============FRECUEENCIA DE CADA CHAR DEL MENSAJE ====================
--sort mensaje = "  HMadllnoou"
-- orden: 1: espacios, 2: mayusculas, 3 orden alfabetico
--group "  HMadllnoou"
-- Resultado: [" ","H","M","a","d","ll","n","o","ou"]
calcularFrecuencia :: (Eq a, Ord a) => [a] -> [(a, String)]
calcularFrecuencia [] = []
calcularFrecuencia mensaje = map procesarGrupo (group (sort mensaje)) where
  procesarGrupo [] = error "grupo vacio"
  procesarGrupo (x:xs) = (x, show (1 + length xs))



compresion = calcularFrecuencia mensaje

-- =================CODIFICACION DE UN MENSAJE =============================

data Huffman =  Hoja Char
              | Rama Huffman Huffman
              deriving (Eq, Ord, Show)

type TablaCodigo = [(Char, String)]
ejemploTablaCodigo = [('o', "0"), ('l',"10"),('r',"11")]

buscarCodigo :: TablaCodigo -> Char -> String
buscarCodigo ((x,y):xs) c | x == c    = y 
                    | otherwise = buscarCodigo xs c


{-
data Tree a = Node a (Tree a) (Tree a)
            | leaf a Int
            | EmptyTree deriving (Show)

constructTree :: (Eq a, Ord a) => [(a, Int)] -> Tree a
constructTree [] = EmptyTree
constructTree [(a, x)] = 

-}
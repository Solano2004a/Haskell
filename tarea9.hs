--Tarea 09
--Usando las diapositivas y los scripts de clase realizar:

--1.- Defina un tipo de datos y hagalo instancia de la clase Scalable
data Rectangle = Rectangle { width :: Double, height :: Double }

--2.- Piense acerca de nociones genéricas (como escalado)
La noción de escalado se refiere a cambiar las dimensiones de un objeto proporcionalmente. En el caso de un rectángulo, podemos pensar en la noción de escalar sus dimensiones multiplicándolas por un factor de escala.
--3.- Defina un tipo de clase con al menos una operación primitiva
class Scalable a where
    scale :: Double -> a -> a

--4.- Piense en las instancias para esa clase
instance Scalable Rectangle where
    scale factor (Rectangle w h) = Rectangle (w * factor) (h * factor)

--5.- Piense en las operaciones derivadas usando esa clase de tipo
-- Ejemplo de uso:
rect1 = Rectangle 2 3      
scaledRect = scale 2 rect1

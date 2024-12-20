--Estimar valores medios entre puntos conocidos (Funcion media)
data Point = Pt Float Float
deriving instance Show Point
{-
data Media = M Point Point
deriving instance Show Media
-}

class MediaPuntos p where
  mediapuntos :: p -> p -> p

instance MediaPuntos Point where
  mediapuntos (Pt x1 y1) (Pt x2 y2) = Pt ((x1 + x2)/2) ((y1+y2)/2)
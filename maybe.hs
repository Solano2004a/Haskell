find::(a->Bool)->[a]->Maybe a
find _ [] = Nothing
find p (x:xs) |p x        = Just x
              |otherwise  = find p xs

isJust::Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

elemento::Eq a => a -> [a] -> Bool
elemento x = isJust . find (==x)
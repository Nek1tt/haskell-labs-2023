--- maybe, recursion, structs ---

--- task 1

quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c = if d < 0 then Nothing else Just (x, y)
    where d = b * b - 4 * a * c
          x = (-b + sqrt d) / (2 * a)
          y = (-b - sqrt d) / (2 * a)

--- task 2

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_:x) = Just x

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit [_] =  Just []
maybeInit (x:xs) = Just (x : maybeInit xs)

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind p [] = Nothing
maybeFind p (x:xs)
    | p x = Just x
    | otherwise = maybeFind p xs

--- task 3


--- task 4.1







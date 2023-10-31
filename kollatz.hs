kollatztoBase :: Int -> Int
kollatztoBase n
    | n == 1 = 0
    | (n `mod` 2) == 0 = 1 + kollatztoBase (n `div` 2)
    | (n `mod` 2) /= 0 = 1 + kollatztoBase (3 * n + 1)

kollatzMax :: Int -> Int -> Int
kollatzMax 1 acc = acc
kollatzMax n acc
    | (n `mod` 2) == 0 = kollatzMax (n `div` 2) (acc `max` n)
    | otherwise = kollatzMax (3 * n + 1) (acc `max` n)
-- Tasks per line

-- Mathematical

-- 1

valueToBin :: Int -> String
valueToBin 0 = "0"
valueToBin 1 = "1"
valueToBin n = valueToBin (n `div` 2) ++ show (n `mod` 2)

-- 2

valueToDec :: Int -> [Int] -> Int
valueToDec n xs = foldl (\acc x -> acc * n + x) 0 xs

-- 3

parsing :: String -> Int
parsing (x:xs) = read (x:xs) :: Int

-- 4

findMissed :: [Int] -> Int
findMissed xs = sum [1..(maximum xs)] - sum (xs)

-- Other

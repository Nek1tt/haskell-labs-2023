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
parsing str = foldl (\acc x -> acc * 10 + fromEnum x - 48) 0 str

-- 4

findMissed :: [Int] -> Int
findMissed xs = sum [1..(maximum xs)] - sum (xs)

-- Other

isTrueOrFalse :: String -> Bool
isTrueOrFalse str = if head str == ')' then False else isTrue str 0
            where
                isTrue :: String -> Int -> Bool
                isTrue [] acc = acc == 0
                isTrue xs acc
                    | head xs == '(' = isTrue (tail xs) (acc + 1)
                    | head xs == ')' = isTrue (tail xs) (acc - 1)
                    | otherwise = isTrue xs acc
            



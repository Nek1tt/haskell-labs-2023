myCount :: Int -> Int
myCount 0 = 0
myCount n = 1 + myCount (n `div` 10)

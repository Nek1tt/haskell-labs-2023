mySum :: Int -> Int
mySum n = if (n < 10) then n else (n `mod` 10) + mySum (n `div` 10)

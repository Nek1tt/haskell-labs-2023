myPower :: Int -> Bool
myPower n
    | n == 0 = False
    | n == 1 = False
    | n == 2 = True
    | otherwise = if (n `mod` 2 == 0) then myPower (n `div` 2) else False
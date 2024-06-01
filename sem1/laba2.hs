head' :: [Int] -> Int
head' (x:_) = x

tail' :: [Int] -> [Int]
tail' (_:x) = x

last' :: [Int] -> Int
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

init' :: [Int] -> [Int]
init' [_] = []
init' (x:xs) = x : init' xs

length' :: [Int] -> Int
length' [] = 0
length' [_] = 1
length' (x:xs) = 1 + length' xs

null' :: [Int] -> Bool
null' x
    | x == [] = True
    | otherwise = False

reverse' :: [Int] -> [Int] 
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

drop' :: Int -> [Int] -> [Int]
drop' _ [] = []
drop' 0 x = x
drop' n (_:xs) = drop' (n - 1) xs

sum' :: [Int] -> Int
sum' [] = 0
sum' [x] = x
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 0
product' [x] = x
product' (x:xs) = x * product' xs

elem' :: Int -> [Int] -> Bool
elem' n [] = False
elem' n (x:xs)
    | n == x = True
    | otherwise = elem' n xs

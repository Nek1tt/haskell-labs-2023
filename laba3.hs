--- List comperh --- 

--- 1

fizzbuzz :: [String]
fizzbuzz = [if (x `mod` 15) == 0 then "fizzbuzz" else if (x `mod` 3 ) == 0 then "fizz" else if (x `mod` 5) == 0 then "fuzz" else show(x) | x <- [1..20]]

--- 2

dotsInCircle :: (Double, Double) -> Double -> [(Double, Double)] -> [(Double, Double)]
dotsInCircle center radius points = [(x, y) | (x, y) <- points , (x - fst center)^2 + (y - snd center)^2 <= radius^2]

--- 3

setAnd :: [Int] -> [Int] -> [Int]
setAnd xs ys = [x | x <- xs, y <- ys, x == y]

--- Recursion, co-Recursion, accumulator ---

--- 1

mySum :: Int -> Int
mySum n = if (n < 10) then n else (n `mod` 10) + mySum (n `div` 10)

--- 2

myCount :: Int -> Int
myCount 0 = 0
myCount n = 1 + myCount (n `div` 10)

--- 3

myPower :: Int -> Bool
myPower n
    | n == 0 = False
    | n == 1 = False
    | n == 2 = True
    | otherwise = if (n `mod` 2 == 0) then myPower (n `div` 2) else False

--- 4

sequenceByPred :: (a -> a) -> a -> [a]
sequenceByPred f x = x : sequenceByPred f (f x)

--- 5

sequenceByTwoPred :: (a -> a -> a) -> a -> a -> [a]
sequenceByTwoPred f x y = x : y : zipWith f (sequenceByTwoPred f x y) (tail (sequenceByTwoPred f x y))

--- 6

--- a
 
kollatztoBase :: Int -> Int
kollatztoBase n
    | n == 1 = 0
    | (n `mod` 2) == 0 = 1 + kollatztoBase (n `div` 2)
    | (n `mod` 2) /= 0 = 1 + kollatztoBase (3 * n + 1)

--- b

kollatzMax :: Int -> Int -> Int
kollatzMax 1 acc = acc
kollatzMax n acc
    | (n `mod` 2) == 0 = kollatzMax (n `div` 2) (acc `max` n)
    | otherwise = kollatzMax (3 * n + 1) (acc `max` n)
 
--- c

kollatz :: Int -> [Int]
kollatz n
    | n == 1 = []
    | otherwise = if (n `mod` 2 == 0) then n : kollatz (n `div` 2) else n : kollatz (3 * n + 1)


--- 7

inAccurateLog :: Int -> Int 
inAccurateLog n = calculateLog n 1
    where calculateLog n k
            | (2 ^ k) > n = k
            | otherwise = calculateLog n (k + 1)

--- 8

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
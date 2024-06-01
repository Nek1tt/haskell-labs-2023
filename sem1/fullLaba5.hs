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
maybeInit (x:xs) = Just (init' (x:xs))
    where init' :: [a] -> [a]
          init' [_] = []
          init' (x:xs) = (x : init' xs)

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind p [] = Nothing
maybeFind p (x:xs)
    | p x = Just x
    | otherwise = maybeFind p xs

--- task 3

data DogBreed = GoldenRetrievers
              | BostonTerriers
              | LabradorRetrievers
              | Poodles
              | BorderCollie
              | Beagle
              | IrishSetter
              | Staffordshire
              | Bull
              | Terrier
    deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data Dog = Dog { name :: String
               , age :: Int
               , gender :: Gender
               , breed :: DogBreed
               , isGoodBoy :: Bool
               } deriving (Show, Eq)

dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie False
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]

-- dogs which are good boys

goodBoys :: [Dog]
goodBoys = filter (\dog -> isGoodBoy dog == True) dogs

-- dogs with name longer than 7 symbols

longNamedDogs :: [Dog]
longNamedDogs = filter (\dog -> length (name dog) > 7) dogs

-- among dogs, which is the most popular gender?

mostPopularDogGender :: Gender
mostPopularDogGender = topGender dogs
    where 
        topGender :: [Dog] -> Gender
        topGender dogs = if (length (filter (\dog -> gender dog == Male) dogs)) > (length (filter (\dog -> gender dog == Female) dogs)) then Male else Female

oldestDog :: [Dog]
oldestDog = filter (\dog -> age dog == oldDog dogs) dogs
    where
        oldDog :: [Dog] -> Int
        oldDog dogs = maximum ([age dog | dog <- dogs])


averageDogAge :: Double
averageDogAge = average dogs
    where
        average :: [Dog] -> Double
        average dogs = fromIntegral (sum [age dog | dog <- dogs]) / (fromIntegral (length dogs))

-- finds dogs with given breed

dogsByBreed :: DogBreed -> [Dog]
dogsByBreed p = filter (\dog -> breed dog == p) dogs

--- task 4.1

data Complex = Complex Double Double

addComplex :: Complex -> Complex -> Complex
addComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

subComplex :: Complex -> Complex -> Complex
subComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)

multiplyComplex :: Complex -> Complex -> Complex
multiplyComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)

divisionComplex :: Complex -> Complex -> Complex
divisionComplex (Complex r1 i1) (Complex r2 i2) = Complex ((r1 * r2 + i1 * i2) / (r1 * r1 + i2 * i2)) ((i1 * r2 - r1 * i2) / (r1 * r1 + i2 * i2))

conjugateComplex :: Complex -> Complex
conjugateComplex (Complex r i) = (Complex r ((-1) * i))

absoluteComplex :: Complex -> Double
absoluteComplex (Complex r i) = sqrt (r * r + i * i)

--- task 5

data MyList a = MyList a (MyList a) | EmptyList
    deriving (Show)

fromList :: [a] -> MyList a
fromList [] = EmptyList
fromList (x:xs) = MyList x (fromList xs)

toList :: MyList a -> [a]
toList EmptyList = []
toList (MyList x xs) = x : toList xs

reverseMyList :: MyList a -> MyList a
reverseMyList list = reverseMyListAnother list EmptyList
    where
        reverseMyListAnother :: MyList a -> MyList a -> MyList a
        reverseMyListAnother EmptyList acc = acc
        reverseMyListAnother (MyList x xs) acc = reverseMyListAnother xs (MyList x acc)

-- should do the same thing as standard map

mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList p EmptyList = EmptyList
mapMyList p (MyList x xs) = MyList (p x) (mapMyList p xs)
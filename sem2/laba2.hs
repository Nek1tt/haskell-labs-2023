
import Control.Monad.State


type GreekData = [(String, [Integer])]

greekDataA :: GreekData
greekDataA = [ ("alpha", [5, 10])
             , ("beta", [0, 8])
             , ("gamma", [18, 47, 60])
             , ("delta", [42])
             ]

greekDataB :: GreekData
greekDataB = [ ("phi", [53, 13])
             , ("chi", [21, 8, 191])
             , ("psi", [])
             , ("omega", [6, 82, 144])
             ]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

divMay :: Double -> Double -> Maybe Double
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: (Ord a) => [a] -> Maybe a
maximumMay [] = Nothing 
maximumMay xs = findMax xs Nothing 
    where  
        findMax :: (Ord a) => [a] -> Maybe a -> Maybe a
        findMax [] acc = acc 
        findMax (y:ys) acc
            | (Just y) > acc = findMax ys (Just y)
            | otherwise = findMax ys acc

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

{-
 implement the function WITHOUT do-notation or any monad magic. only pattern-matching, where and let in
 first query the GreekData that is passed in,
 look up the string passed in the second argument,
 and retrieve the corresponding list of Integers. Call this list xs.
 Next calculate the maximum of the tail of xs
 (Don’t use any pattern matching here.
 Use case expressions and the maximumMay and tailMay functions)
 Take the maximum and divide it by the head of the list (using headMay and divMay functions).
 If any of these operations along the way return Nothing, then your function should return Nothing.
 But if everything succeeds, then return the final quotient.
 One hint… you’ll need to use the fromIntegral function to convert your two Integers to Doubles for the final call to divMay.
-}

queryGreek :: GreekData -> String -> Maybe Double
queryGreek dt key = case lookup key dt of
    Nothing -> Nothing
    Just xs -> case tailMay xs of 
        Nothing -> Nothing
        Just ys -> case maximumMay ys of 
            Nothing -> Nothing
            Just m -> case headMay xs of 
                Nothing -> Nothing
                Just h -> fromIntegral m `divMay` fromIntegral h
            

-- queryGreek greekDataA "alpha" == Just 2.0

-- Now do the same whole thing, but using do-notation, since Maybe is a Monad
queryGreekPro :: GreekData -> String -> Maybe Double
queryGreekPro dt key = do
            xs <- lookup key dt
            myTail <- tailMay xs
            myMax <- maximumMay myTail
            myHead <- headMay xs
            h <- fromIntegral myMax `divMay` fromIntegral myHead
            return h

-- * a harder task. rewrite queryGreekPro, but without the do-notation, only using the (>>=) operator and its friends
-- in other words, desugarize your notation
queryGreekProPlus :: GreekData -> String -> Maybe Double
queryGreekProPlus dt key = 
    lookup key dt >>= \xs -> 
    tailMay xs >>= \myTail -> 
    maximumMay myTail >>= \myMax -> 
    headMay xs >>= \myHead -> 
    fromIntegral myMax `divMay` fromIntegral myHead

-- state monad

type RandState = [Int]

rollDice :: State RandState Int
rollDice = do
    currentState <- get
    put (tail currentState)
    return $ head currentState

game :: State RandState String
game = do
    firstPlayerRes <- rollDice
    secondPlayerRes <- rollDice
    return $ if firstPlayerRes > secondPlayerRes then "First wins" else if firstPlayerRes < secondPlayerRes then "Second wins" else "it's a draw"

runGame :: String
runGame = evalState game startSeed
    where startSeed = [4, 5, 5, 2, 1, 6]

-- see this program as example:
{-
x=foo
y=bar
y=$x
l=l
x=$y
-}
{-
At the end of this program the state is:
x = foo
y = foo
l = l
-}

exampleProgram :: String
exampleProgram = "x=foo\ny=bar\ny=$x\nl=l\nx=$y"

-- one of possible answers. order in list doesn't matter
exampleAns :: [(String, String)]
exampleAns = [("x", "foo"), ("y", "foo"), ("l", "l")]

check :: IO ()
check = do
    let resultState = solveState exampleProgram
    if exampleAns `listEq` resultState
        then putStrLn "OK!"
        else error "something wrong:("
    where listEq l r = leftInRight && rightInLeft
            where leftInRight = all (\x -> x `elem` r) l
                  rightInLeft = all (\x -> x `elem` l) r

data Value = Literal String | VariableReference String
    deriving (Show)

data Command = Command { varName :: String, whatToPut :: Value }
    deriving (Show)

-- you can choose something else!
type InterpreterState = [(String, String)]

solveState :: String -> [(String, String)]
solveState input = interpretToState (map parse $ lines input)

-- example: "foo=bar" -> Command "foo" (Literal "bar")
-- example: "foo=$bar" -> Command "foo" (VariableReference "bar")
parse :: String -> Command 
parse x = let (varName, whatToPut) = break (== '=') x
              whatToPut' = if head (drop 1 whatToPut) == '$' then VariableReference (tail (drop 1 whatToPut)) else Literal (tail whatToPut)
          in Command varName whatToPut'

-- you may rewrite this. e.g. you can use fold
-- but if you look at standard library there might be
-- a better alternative for chaining state functions.
-- In other words, executing a list of (State s a)
-- functions is a common task, and it has a standard implementation
interpretMany :: [Command] -> State InterpreterState ()
interpretMany [] = return ()
interpretMany (x:xs) = do
    interpretOne x
    interpretMany xs

-- using get, set and other State functions, interpret the command
interpretOne :: Command -> State InterpreterState ()
interpretOne (Command varName (Literal value)) = do
    currentState <- get
    put ((varName, value) : currentState)

interpretOne (Command varName (VariableReference refName)) = do
    currentState <- get
    let refValue = lookup refName currentState
    case refValue of
        Just value -> do
            let updatedState = (varName, value) : filter (\(name, _) -> name /= varName) currentState 
            put updatedState
            
-- you can choose other type for result
interpretToState :: [Command] -> [(String, String)]
interpretToState commands = execState (interpretMany commands) emptyState
    where emptyState = []
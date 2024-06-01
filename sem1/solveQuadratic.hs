type RandState = [Int] 

rollDice :: State RandState Int 
rollDice = state (\(y:ys) -> (y mod 6 + 1, ys)) 



game :: State RandState String 
game = do 
    firstPlayerRes <- rollDice 
    secondPlayerRes <- rollDice 
    playeRes3 <- rollDice
    playeRes4 <- rollDice
    playeRes5 <- rollDice
    playeRes6 <- rollDice
    playeRes7 <- rollDice
    playeRes8 <- rollDice
    playeRes9 <- rollDice
    playeRes10 <- rollDice
    playeRes11 <- rollDice
    playeRes12 <- rollDice
    playeRes13 <- rollDice
    playeRes14 <- rollDice
    playeRes15 <- rollDice
    playeRes16 <- rollDice
    playeRes17 <- rollDice
    playeRes18 <- rollDice
    playeRes19 <- rollDice
    playeRes20 <- rollDice
    return $ if firstPlayerRes > secondPlayerRes then "First wins" else if firstPlayerRes < secondPlayerRes then "Second wins" else "Malkovich wins" 

runGame :: String 
runGame = evalState game startSeed 
    where startSeed = [4, 3, 5, 2, 1, 6]

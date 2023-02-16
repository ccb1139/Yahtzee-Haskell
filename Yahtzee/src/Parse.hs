module Parse where

import Score
import Data.Char (ord)
import Text.Printf

gameStart::IO [ScoreCard]
gameStart = do
    putStrLn("\n===========================\n")
    putStrLn("Welcome to Yahtzee!")
    putStrLn("\n===========================\n")
    pg <- promptPlayerYorN "Would you like to start a game? (y/n)"
    if (pg == True) 
        then do
            pScards <- getNumPlayers
            return (pScards)
    else
        return ([])

getNumPlayers::IO [ScoreCard]
getNumPlayers = do
    putStrLn("\nHow many players?")
    putStr("1.) Two Players\n2.) Three Players\n3.) Four Players\n\n")
    input <- getLine
    if((isInt input) == True && ((read input) > 0 && (read input) < 4)) 
        then do
            players <- getPlayerScArr (read input)
            return(players)
    else do
        putStrLn("Please enter a number from the list!")
        getNumPlayers 
        
getPlayerScArr::Int -> IO [ScoreCard]
getPlayerScArr 1 = do
    p1 <- getScoreCard
    p2 <- getScoreCard
    return ([p1, p2])
getPlayerScArr 2 = do
    p1 <- getScoreCard
    p2 <- getScoreCard
    p3 <- getScoreCard
    return ([p1, p2, p3])
getPlayerScArr 3 = do
    p1 <- getScoreCard
    p2 <- getScoreCard
    p3 <- getScoreCard
    p4 <- getScoreCard
    return ([p1, p2, p3, p4])


parseReRollValid::[Int] -> IO [Int]
parseReRollValid rtnArr = do
    input <- getLine
    intList <- parseReRoll input rtnArr
    if((length intList) > 5) 
        then do 
            putStrLn("Please only enter up to 5 index numbers!")
            parseReRollValid rtnArr
    else
        return intList

parseReRoll::[Char] -> [Int] -> IO [Int]
parseReRoll [] numArr = return(numArr)
parseReRoll (r:rr) numArr = do
    let cTOi = parseReRollHelper r
    if (cTOi /= - 1)
        then parseReRoll rr (numArr ++ [cTOi])
    else parseReRoll (rr) (numArr)


parseReRollHelper:: Char -> Int
parseReRollHelper '0' =  0
parseReRollHelper '1' =  1
parseReRollHelper '2' =  2
parseReRollHelper '3' =  3
parseReRollHelper '4' =  4
parseReRollHelper _ = -1

promptPlayerYorN:: String -> IO Bool
promptPlayerYorN prompt = do
    -- print(prompt) 
    printf "%s%s%s" "\n" prompt "\n"
    input <- getLine
    if (input == "y") then return True
    else if (input == "n" ) then return False
    else do
        printf "%s" "\nPlease enter a valid input (y/n)\n"
        -- print("Please enter a valid input (y/n)")
        promptPlayerYorN prompt

isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False

parsePlayerScChoice:: [ScoreType] -> IO ScoreType
parsePlayerScChoice sOpts = do
    -- printf "%s" "Choose a slot to fill!"
    putStrLn("\nChoose a slot to fill!")
    pInp <- getLine
    -- let curSOpt = printScHelper sOPt 
    -- If the player choice is an int
    if ((isInt pInp) == True ) then
        -- If the player choice is an avaible choice
        (if ((read pInp) < ((length sOpts) + 1) && (read pInp) > 0) 
            -- Return the ScPair at the correct index
            then do
                -- print(sOpts!!((read pInp) - 1))
                let pickedScore = printScHelper (sOpts!!((read pInp) - 1))
                -- print(snd pickedScore)
                if ((snd pickedScore) == 0)
                    then return (ScPair (fst pickedScore) (-1))
                else
                    return (sOpts!!((read pInp) - 1))        
                
        else do
            printf "%s" "List index is out of range! Please choose an avaible option"
            parsePlayerScChoice sOpts
            )
    else do
        printf "%s" "Please enter a number!"
        parsePlayerScChoice sOpts


contConfm::Bool -> IO ()
contConfm True = return ()
contConfm validCont = do
    cnt <- promptPlayerYorN "Press y to continue:"
    if (cnt == True)
        then contConfm True
    else 
        contConfm False
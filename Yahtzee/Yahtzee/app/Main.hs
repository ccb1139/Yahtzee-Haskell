module Main where
    
import System.Random
import Lib
import Dice
import Score
import Data.List

-- Example program
-- https://hackage.haskell.org/package/hyahtzee-0.5/src/

main :: IO ()
main = do
    p1Hand <- getHand
    print(p1Hand)
    pRR <- getPReRoll
    print (pRR)
    print(p1Hand)
    print("Re-rolling:")
    p1Hand <- reRollDice pRR p1Hand

    -- p1ScoreCard <- genScoreCard
    -- test <- reRollDice [0, 4] p1Hand
    -- pInput <- getPInput
    -- print(pInput)

    
    print(p1Hand)
    -- print(p1ScoreCard)



getPReRoll::IO [Int]
getPReRoll= do
    print("What would you like to re-roll?")
    input <- getLine
    reRollInds <- parseReRoll input []
    
    return (reRollInds)

-- Probably need input validation
parseReRoll::[Char] -> [Int] -> IO [Int]
parseReRoll [] numArr = return(numArr)
parseReRoll rr numArr = do
    -- print("rr & numArr:")
    -- print(rr)
    -- print(numArr)
    -- print("-----")
    let r = head rr
    let remainR = tail rr
    let cTOi = parseReRollHelper r
    if (cTOi /= - 1)
        then parseReRoll remainR (numArr ++ [cTOi])
    else parseReRoll (remainR) (numArr)

    

parseReRollHelper:: Char -> Int
parseReRollHelper '0' =  0
parseReRollHelper '1' =  1
parseReRollHelper '2' =  2
parseReRollHelper '3' =  3
parseReRollHelper '4' =  4
parseReRollHelper _ = -1

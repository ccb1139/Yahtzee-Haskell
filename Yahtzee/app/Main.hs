module Main where

import Control.Monad
import System.Random
import Dice
import Score
import Parse
import Data.List
import Text.Printf

main :: IO ()
main = do

    gameLoop True
    
    print("Game Over")


gameLoop::Bool -> IO()
gameLoop False = return ()
gameLoop True = do
    players <- gameStart
    if ((length players) == 0)
        then gameLoop False
    else do
        finalScores <- playYahtzee players ((length players) * 13)
        winner <- checkWinner finalScores 0 1 ([], [])
        printWinner winner
        pgq <- promptPlayerYorN "Play again? (y/n)"
        if (pgq == True) 
            then gameLoop True
        else
            gameLoop False
        


-- Turns must be times num of players times the num of turns
playYahtzee::[ScoreCard] -> Int -> IO [ScoreCard]
playYahtzee playerScs 0 = return playerScs
playYahtzee (curP:nextPs) turns = do

    -- Logic for displaying the correct players turn, took forever for some reason
    let eq1 = (((length ([curP] ++ nextPs))*13) - turns)
    let eq2 = (length ([curP] ++ nextPs))
    let eq3 = ((eq1 `mod` eq2))

    if(eq3 == 0)
        then do
            printNewRoundHeader ((eq1 `div` eq2) + 1)
            printNewTurnHeader (eq3 + 1)
    else 
        printNewTurnHeader (eq3 + 1)
    putStrLn("Current Card: \n(Slots occupied with a zero are marked with -1)")
    printScoreCard curP
    newPCard <- playYahtzeeTurn curP

    contConfm False
    
    playYahtzee (nextPs ++ [newPCard]) (turns - 1)
    
printNewRoundHeader::Int -> IO()
printNewRoundHeader curRound= do
    putStrLn("\n===========================\n")
    printf "Round %d / 13\n" curRound   
    putStrLn("\n===========================\n")

printNewTurnHeader:: Int -> IO ()
printNewTurnHeader playerTurn = do
    putStrLn("\n===========================\n")
    printf "Player %d's turn!\n" playerTurn
    putStrLn("\n===========================\n")

playYahtzeeTurn:: ScoreCard -> IO ScoreCard
playYahtzeeTurn p1SCard= do

    p1SCard <- playerTurn p1SCard

    return p1SCard

playerTurn:: ScoreCard -> IO ScoreCard
playerTurn curPlayerCard = do
    -- Get the players new hand of dice
    unSortedPHand <- getHand
    -- Have the player re-roll
    unSortedPHand <- doPlayerTurn unSortedPHand 3
    -- Sort Player hand for combo testing
    sortedPHand <- sortHandForComboTesting unSortedPHand
    printPHand unSortedPHand

    -- Now do the combo checks
    top <- topCardComboCheck sortedPHand
    btm <- bottomCardComboCheck sortedPHand

    -- Show the avabile options
    opt <- getFullOptions curPlayerCard (top ++ btm)
    showAvailOptions opt 1

    -- Ask the player for their choice and put into score card
    playerChoice <- parsePlayerScChoice opt
    
    let newCard = addToScoreCard playerChoice curPlayerCard

    rtnNewCard <- formatSC newCard
    formatAndPrintSC newCard
    

    return rtnNewCard

    


getPlayerReRoll::IO [Int]
getPlayerReRoll= do
    putStrLn("\nWhich dice would you like to re-roll?")
    putStrLn("Please enter up to 5 index numbers of the dice in your hand")
    putStrLn("To re-roll dice 0 and 4 you would enter: 0 4 or 04\n")
    reRollInds <- parseReRollValid []
    
    return (reRollInds)

printPHand:: Hand -> IO()
printPHand [Face d1, Face d2, Face d3, Face d4, Face d5] =do
    printf "%s" "\nYour current hand:\n"
    printf "%s\t%d %d %d %d %d\n" "Dice Index:" (0 :: Int) (1 :: Int) (2 :: Int) (3 :: Int) (4 :: Int)
    printf "%s\t%d %d %d %d %d\n\n" "Your Hand:" d1 d2 d3 d4 d5

doPlayerTurn::Hand -> Int -> IO Hand
doPlayerTurn pHand 0 = return pHand
doPlayerTurn pHand turnsLeft= do
    printPHand pHand
    -- printf "Current re-rolls left: %d" (show turnsLeft)
    putStrLn ("Current re-rolls left: " ++ show turnsLeft)
    -- print ("Current re-rolls left: " ++ show(turnsLeft))
    pAns <- promptPlayerYorN "Do you want to re-roll any dice (y/n)?"
    if (pAns == True)
        then do
            pRR <- getPlayerReRoll
            pHand <- reRollDice pRR pHand
            doPlayerTurn pHand (turnsLeft - 1)
    else 
        doPlayerTurn pHand 0



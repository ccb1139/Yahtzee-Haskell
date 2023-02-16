module Dice where

import System.Random

data Dice = Face Int deriving (Eq)
instance Show Dice where
    show (Face val) = show val

type Hand = [Dice]

toDie::Int -> Dice
toDie x = Face x 

printHand:: Hand -> IO String
printHand = do
    

newHand:: [Int] -> Hand
newHand xs = map toDie xs

rollDice::IO Int
rollDice = getStdRandom (randomR (1,6))

reRollDie :: Int -> IO [Int]
reRollDie n = sequence $ replicate n $ randomRIO (1,6::Int) 

getHand::IO Hand
getHand = do
    d1 <- rollDice    
    d2 <- rollDice
    d3 <- rollDice
    d4 <- rollDice
    d5 <- rollDice

    let dice = [d1, d2, d3, d4, d5]
    return(newHand dice)

reRollDice:: [Int] -> Hand -> IO Hand 
reRollDice [] npHand = return (npHand)
reRollDice nr npHand = do
    let r = head nr
    let back = tail nr 
    let(x,_:ys) = splitAt r npHand
    newNum <- reRollDie 1
    let newHand = x ++ [toDie (newNum!!0)] ++ ys
    reRollDice back newHand
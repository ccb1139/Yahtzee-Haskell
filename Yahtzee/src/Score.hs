module Score where

import Dice
import Data.List
import Text.Printf



data ScoreType = ScPair String Int 

instance Eq ScoreType where 
    (ScPair n1 v1) == (ScPair n2 v2) = n1 == n2

instance Show ScoreType where
    show (ScPair name val) = show name ++ ": " ++ show val ++ "\n"

type ScoreCard = [ScoreType]

getScoreCard:: IO ScoreCard
getScoreCard = do
    let card = [(ScPair "Ones" 0),          -- 0
                (ScPair "Twos" 0),          -- 1
                (ScPair "Threes" 0),        -- 2
                (ScPair "Fours" 0),         -- 3
                (ScPair "Fives" 0),         -- 4
                (ScPair "Sixes" 0),         -- 5
                (ScPair "Total Score" 0),   -- 6
                (ScPair "Bonus" 0),         -- 7
                (ScPair "Upper Total" 0),   -- 8
                (ScPair "Three of a kind" 0),   -- 9
                (ScPair "Four of a kind" 0),    -- 10
                (ScPair "Full House" 0),        -- 11
                (ScPair "Small Straight" 0),    -- 12      
                (ScPair "Large Straight" 0),    -- 13
                (ScPair "Yahtzee" 0),           -- 14
                (ScPair "Chance" 0),            -- 15
                (ScPair "Lower Total" 0),       -- 16
                (ScPair "Grand Total" 0)]       -- 17

    return (card)

addToScoreCard:: ScoreType -> ScoreCard -> ScoreCard
addToScoreCard newAdd curScoreCard = map (\x -> if x == newAdd then newAdd else x) curScoreCard

printScHelper:: ScoreType -> (String, Int)
printScHelper (ScPair title score) = (title, score)



-- Function converts all -1 to 0
convertToZero :: [(ScoreType)] -> [(ScoreType)]
-- convertToZero card = [(ScPair name value) | (ScPair name value) <- card, value /= -1] ++ [(ScPair name 0) | (ScPair name value) <- card, value == -1]
convertToZero card = [convertPair p | p <- card]
  where convertPair (ScPair name value)
          | value == -1 = (ScPair name 0)
          | otherwise = (ScPair name value)


-- Sums the top
sumFirstSix :: [(ScoreType)] -> Int
sumFirstSix card = sum [if value == -1 then 0 else value | (ScPair name value) <- take 6 card]

-- Sums the bottom
sumItems9to13 :: [(ScoreType)] -> Int
sumItems9to13 card = sum [value | (ScPair name value) <- drop 9 $ take 16 card]

formatAndPrintSC:: ScoreCard -> IO ScoreCard
formatAndPrintSC sc = do
    newSc <- finalFormatSC sc 
    putStrLn("\nYour new Card:\n")
    putStrLn("-------------------------------------------")
    printScoreCard newSc
    putStrLn("-------------------------------------------\n")
    return newSc

printScoreCard:: ScoreCard -> IO ()
printScoreCard [] = return()
printScoreCard sc = do
    let curPair = head sc
    let restPairs = tail sc
    let curPairFormat = printScHelper curPair
    printf "%s: %d \n" (fst curPairFormat) (snd curPairFormat)
    printScoreCard restPairs

finalFormatSC:: ScoreCard -> IO ScoreCard
-- formatSC sc 16 = return
finalFormatSC sc = do
    let cardNoZeros = convertToZero sc
    
    let topSum = sumFirstSix cardNoZeros
    let bottomSum = sumItems9to13 cardNoZeros

    if(topSum > 62) 
        then do
            let bonus = 35
            let wScoreTot = addToScoreCard (ScPair "Total Score" topSum) cardNoZeros
            let wBonus = addToScoreCard (ScPair "Bonus" (35)) wScoreTot
            let wUpperScoreTot = addToScoreCard (ScPair "Upper Total" (topSum + bonus)) wBonus

            let wLowerTotal = addToScoreCard (ScPair "Lower Total" bottomSum) wUpperScoreTot
            let finalCard = addToScoreCard (ScPair "Grand Total" ((topSum + bonus) + bottomSum)) wLowerTotal
            return(finalCard)
    else do
        let bonus = 0
        let wScoreTot = addToScoreCard (ScPair "Total Score" topSum) cardNoZeros
        let wBonus = addToScoreCard (ScPair "Bonus" (0)) wScoreTot
        let wUpperScoreTot = addToScoreCard (ScPair "Upper Total" topSum) wBonus

        let wLowerTotal = addToScoreCard (ScPair "Lower Total" bottomSum) wUpperScoreTot
        let finalCard = addToScoreCard (ScPair "Grand Total" ((topSum + bonus) + bottomSum)) wLowerTotal
        return (finalCard)

-- Takes a scorecard and an index
formatSC:: ScoreCard -> IO ScoreCard
-- formatSC sc 16 = return
formatSC sc = do
    let cardNoZeros = sc
    
    let topSum = sumFirstSix cardNoZeros
    let bottomSum = sumItems9to13 cardNoZeros

    if(topSum > 62) 
        then do
            let bonus = 35
            let wScoreTot = addToScoreCard (ScPair "Total Score" topSum) cardNoZeros
            let wBonus = addToScoreCard (ScPair "Bonus" (35)) wScoreTot
            let wUpperScoreTot = addToScoreCard (ScPair "Upper Total" (topSum + bonus)) wBonus

            let wLowerTotal = addToScoreCard (ScPair "Lower Total" bottomSum) wUpperScoreTot
            let finalCard = addToScoreCard (ScPair "Grand Total" ((topSum + bonus) + bottomSum)) wLowerTotal
            return(finalCard)
    else do
        let bonus = 0
        let wScoreTot = addToScoreCard (ScPair "Total Score" topSum) cardNoZeros
        let wBonus = addToScoreCard (ScPair "Bonus" (0)) wScoreTot
        let wUpperScoreTot = addToScoreCard (ScPair "Upper Total" topSum) wBonus

        let wLowerTotal = addToScoreCard (ScPair "Lower Total" bottomSum) wUpperScoreTot
        let finalCard = addToScoreCard (ScPair "Grand Total" ((topSum + bonus) + bottomSum)) wLowerTotal
        return (finalCard)




showAvailOptions:: [ScoreType] -> Int -> IO()
showAvailOptions [] _ = return()
showAvailOptions card ind = do
    let curPairFmt = printScHelper (head card)
    printf "%d.) %s: %d\n" ind (fst curPairFmt) (snd curPairFmt)
    showAvailOptions (tail card) (ind + 1)

checkIndvScore::ScoreCard -> String -> ScoreType
checkIndvScore sc sType = case find(\(ScPair name val) -> name == sType) sc of
    Nothing -> ScPair "INVALID SEARCH" (-10)
    Just x -> x


altEqFunc::ScoreType -> ScoreType -> Bool
altEqFunc (ScPair name1 value1) (ScPair name2 value2) = name1 == name2 && value1 == value2
 
getFullOptions::ScoreCard -> [ScoreType] -> IO [ScoreType]
getFullOptions curCardAll curHandSc = do
    availOpt <- _getAvailOptions curCardAll curHandSc []
    if((length availOpt) == 0) 
        then do
            availOpt <- getUnAvailOptions curCardAll curHandSc []
            return availOpt
    else
        return availOpt

_getAvailOptions:: ScoreCard -> [ScoreType] -> [ScoreType] -> IO [ScoreType]
_getAvailOptions curCard [] optToPrint = return optToPrint        
_getAvailOptions curCard (cHP:cHPs) optToPrint = do
    let _curPair = printScHelper cHP
    -- print((altEqFunc (checkIndvScore curCard (fst _curPair)) (ScPair (fst _curPair) (0))))
    if ( (altEqFunc (checkIndvScore curCard (fst _curPair)) (ScPair (fst _curPair) (0))) && ((snd(_curPair) /= 0)))
        then _getAvailOptions curCard cHPs (optToPrint ++ [cHP])
    else _getAvailOptions curCard cHPs (optToPrint)

getUnAvailOptions:: ScoreCard -> [ScoreType] -> [ScoreType] -> IO [ScoreType]
getUnAvailOptions curCard [] [] = return ([])
getUnAvailOptions curCard [] unOptToPrint = return (unOptToPrint)
getUnAvailOptions curCard (cp:curHandOpt) unOptToPrint = do
    let _curPair = printScHelper cp
    let _curScPair = printScHelper (checkIndvScore curCard (fst _curPair))
    if((snd _curPair) == 0 && (snd _curScPair) == 0)
        then  getUnAvailOptions curCard curHandOpt (unOptToPrint ++ [cp])
    else
         getUnAvailOptions curCard curHandOpt unOptToPrint

-- Takes all the score cards, the highest score, the winning scorecard
-- Returns a list, if multiple then there was a tie
-- Second int is to keep track of which player wins
checkWinner:: [ScoreCard] -> Int -> Int -> ([ScoreCard], [Int]) -> IO ([ScoreCard], [Int])
checkWinner [] highest highPNumber (highestSCs, player) = return (highestSCs, player)
checkWinner (curCard:restCards) highest highPNumber (highestSCs, player) = do
    let curGT = printScHelper (checkIndvScore curCard "Grand Total")
    if((snd curGT) > highest) 
        then checkWinner restCards (snd curGT) (highPNumber + 1) ([curCard], [highPNumber])
    else if ((snd curGT) == highest)
        then checkWinner restCards (snd curGT) (highPNumber + 1) ((highestSCs ++ [curCard]), (player ++ [highPNumber]))
    else
        checkWinner restCards highest (highPNumber + 1) (highestSCs, player)

tiedPlayersHelper::[Int] -> String
tiedPlayersHelper tiedPs = concat ["- Player " ++ show tiedP ++ "\n"| tiedP <- tiedPs]

tiedScHelper::[ScoreCard] -> [Int] -> IO ()
tiedScHelper [] [] = return ()
tiedScHelper (sc:scs) (p:ps) = do
    printf "Player: %d\n" p
    printScoreCard sc
    putStrLn("\n")
    tiedScHelper scs ps

printWinner::([ScoreCard], [Int]) -> IO ()
printWinner (winningSc, winningPNum) = do
    if((length winningSc) == 1) 
        then do
            putStrLn("\n===========================\n")
            putStrLn("It appears we have a winner!")
            printf "Player %d has won!!!\n\n" (winningPNum!!0)
            putStrLn("The winning score card:")
            printScoreCard (winningSc!!0)
            putStrLn("\n===========================\n")
    else if ((length winningSc) > 1 ) 
        then do
            putStrLn("\n===========================\n")
            putStrLn("It appears we have a tie!")
            printf "Tied players:\n%s\n\n" (tiedPlayersHelper winningPNum)
            putStrLn("The tied score cards:")
            tiedScHelper winningSc winningPNum
            putStrLn("\n===========================\n")
    else do
        putStrLn("\n===========================\n")
        putStrLn("It appears there is no winner!")
        putStrLn("\n===========================\n")


diceValue:: Dice -> Int
diceValue (Face x) = x

handValue:: Hand -> Int
handValue [] = 0
handValue (die:pHand) = diceValue die + handValue pHand

reverseHand :: Hand -> Hand
reverseHand = \pHand ->
    case pHand of
        [] -> []
        x:xs -> reverseHand xs ++ [x]

-- Sorts and reverses array for highest possible combo every time
sortHandForComboTesting::Hand -> IO Hand
sortHandForComboTesting pHand = do
    let pHandSorted = sort pHand
    let phsReversed = reverseHand pHandSorted

    return (phsReversed)

countDieType:: Hand -> Int -> Int
countDieType pHand die = sum (filter(== die) (toInts pHand))


topCardComboCheck:: Hand -> IO [ScoreType]
topCardComboCheck pHand = do
    let oneScore = ones pHand
    let twoScore = twos pHand
    let threeScore = threes pHand
    let fourScore = fours pHand
    let fiveScore = fives pHand
    let sixesScore = sixes pHand

    let topScores = [oneScore, twoScore, threeScore, fourScore, fiveScore, sixesScore]
    return topScores


bottomCardComboCheck:: Hand -> IO [ScoreType]
bottomCardComboCheck pHand = do
    let oneP = onePair pHand
    let twoP = twoPair pHand
    let threeP = threePair pHand
    let fourP = fourPair pHand
    let fiveP = yahtzee pHand 
    let fH = fullHouse pHand
    let smStraight = smallStraight pHand
    let lgStraight = largeStraight pHand
    let _chance = chance pHand

    let btmScores = [threeP, fourP, fiveP, fH, smStraight, lgStraight, _chance]
    return(btmScores)
 

-- Combo Logic

ones::Hand -> ScoreType
ones pHand = ScPair "Ones" (countDieType pHand 1)

twos::Hand -> ScoreType
twos pHand = ScPair "Twos" (countDieType pHand 2)

threes::Hand -> ScoreType
threes pHand = ScPair "Threes" (countDieType pHand 3)

fours::Hand -> ScoreType
fours pHand = ScPair "Fours" (countDieType pHand 4)

fives::Hand -> ScoreType
fives pHand = ScPair "Fives" (countDieType pHand 5)

sixes::Hand -> ScoreType
sixes pHand = ScPair "Sixes" (countDieType pHand 6)

onePair :: Hand -> ScoreType
onePair [Face d1, Face d2, Face d3, Face d4, Face d5]
    | d1 == d2 = ScPair "One pair" (2 * d1)
    | d2 == d3 = ScPair "One pair" (2 * d2)
    | d3 == d4 = ScPair "One pair" (2 * d3)
    | d4 == d5 = ScPair "One pair" (2 * d4)
    | otherwise = ScPair "One pair"  0

twoPair:: Hand -> ScoreType
twoPair [Face d1, Face d2, Face d3, Face d4, Face d5]
    | d1 == d2 && d3 == d4 && d2 /= d3 = ScPair "Two pair" (2 * d1 + 2 * d3)
    | d2 == d3 && d4 == d5 && d3 /= d4 = ScPair "Two pair" (2 * d2 + 2 * d4)
    | d1 == d2 && d5 == d4 && d2 /= d5 = ScPair "Two pair" (2 * d1 + 2 * d5)
    | otherwise = ScPair "Two pair" 0

threePair:: Hand -> ScoreType
threePair [Face d1, Face d2, Face d3, Face d4, Face d5]
    | d1 == d3 && d3 /= d4 = ScPair "Three of a kind" (3 * d1)
    | d2 == d4 && d4 /= d5 = ScPair "Three of a kind" (3 * d2)
    | d3 == d5 && d2 /= d3 = ScPair "Three of a kind" (3 * d3)
    | otherwise = ScPair "Three of a kind" 0

fourPair:: Hand -> ScoreType
fourPair [Face d1, Face d2, Face d3, Face d4, Face d5]
    | d1 == d4 = ScPair "Four of a kind" (4 * d1)
    | d2 == d5 = ScPair "Four of a kind" (4 * d2)
    | otherwise = ScPair "Four of a kind" 0

yahtzee::Hand -> ScoreType
yahtzee [Face d1, Face d2, Face d3, Face d4, Face d5]
    | d1 == d5 = ScPair "Yahtzee" 50
    | otherwise = ScPair "Yahtzee" 0

chance:: Hand -> ScoreType
chance pHand = ScPair "Chance" (sum (toInts pHand))

fullHouse:: Hand -> ScoreType
fullHouse [Face d1, Face d2, Face d3, Face d4, Face d5]
    | d1 == d3 && d4 == d5 && d3 /= d4 = ScPair "Full House" 25
    | d3 == d5 && d1 == d2 && d3 /= d2 = ScPair "Full House" 25
    | otherwise = ScPair "Full House" 0

smallStraight::Hand -> ScoreType
smallStraight [Face d1, Face d2, Face d3, Face d4, Face d5] 
    | d1 == 6 && d2 == 5 && d3 == 4 && d4 == 4 = ScPair "Small Straight" 30
    | d1 == 5 && d2 == 4 && d3 == 3 && d4 == 2 = ScPair "Small Straight" 30
    | d1 == 4 && d2 == 3 && d3 == 2 && d4 == 1 = ScPair "Small Straight" 30
    | d2 == 6 && d3 == 5 && d4 == 4 && d5 == 3 = ScPair "Small Straight" 30
    | d2 == 5 && d3 == 4 && d4 == 3 && d5 == 2 = ScPair "Small Straight" 30
    | d2 == 4 && d3 == 3 && d4 == 2 && d5 == 1 = ScPair "Small Straight" 30
    | otherwise = ScPair "Small Straight" 0

largeStraight::Hand -> ScoreType 
largeStraight [Face d1, Face d2, Face d3, Face d4, Face d5] 
    | d1 == 6 && d2 == 5 && d3 == 4 && d4 == 3 && d5 == 2 = ScPair "Large Straight" 40
    | d1 == 5 && d2 == 4 && d3 == 3 && d4 == 2 && d5 == 1 = ScPair "Large Straight" 40
    | otherwise = ScPair "Large Straight" 0
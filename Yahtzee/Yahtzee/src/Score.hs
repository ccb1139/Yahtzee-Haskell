module Score where

import Dice

data ScoreType = ScPair String Int 
instance Show ScoreType where
    show (ScPair name val) = show name ++ ": " ++ show val ++ "\n"

type ScoreCard = [ScoreType]

diceValue:: Dice -> Int
diceValue (Face x) = x

genScoreCard:: IO ScoreCard
genScoreCard = do
    let card = [(ScPair "Ones" 0),
                (ScPair "Twos" 0),
                (ScPair "Threes" 0),
                (ScPair "Fours" 0),
                (ScPair "Fives" 0),
                (ScPair "Sixes" 0)]

    return (card)



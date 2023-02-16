# Yahtzee Game in Haskell
This is a fully functioning local multiplayer game of Yahtzee made in Haskell. The game is played on the terminal, and can be played by 2-4 players.

## Prerequisites
To run the game, you will need:

Haskell Compiler (GHC)
Stack build tool
Installation
Clone the repository to your local machine using git clone https://github.com/your_username/yahtzee-game.git
cd into the yahtzee-game directory
Run stack build to build the game
Run stack exec yahtzee-game to start the game
## Game Rules
Yahtzee is a dice game where the objective is to score the most points by rolling five dice to make certain combinations.

Each turn, a player can roll the dice up to three times in an attempt to make one of the 13 possible scoring combinations. The scoring combinations are:

Ones
Twos
Threes
Fours
Fives
Sixes
Three of a kind
Four of a kind
Full House
Small Straight
Large Straight
Yahtzee (five of a kind)
Chance (any combination)
After each roll, the player can choose to score the roll in any of the scoring combinations. Once a scoring combination has been used, it cannot be used again for the rest of the game.

The game ends after all players have taken their 13 turns and scored their rolls. The player with the highest total score wins.

## How to Play
Enter the number of players (2-4) at the beginning of the game.
Players take turns rolling the dice and selecting a scoring combination for each roll.
The game will display the current player's scorecard and the possible scoring combinations for each roll.
Enter the number of the scoring combination to use or enter 0 to score a zero in that combination.
Repeat steps 2-4 until all players have completed their 13 turns.
The game will display the final scores and declare the winner.

## Credits
This game was created by Caleb Bergen as a project for Effective Functional Programming.

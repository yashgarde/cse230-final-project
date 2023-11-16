# CSE 230 Final Project - 2048 Game

## Team Members
- Yash Garde
- Shravan Konduru

## Proposal
We are planning to build a working version of the popular game 2048 as TUI using the Haskell `brick` library. 

The game is played on a 4x4 game board where the goal is to get the highest score possible. The players score is determined by the largest numbered tile that is present on the board at any time. To increase their score, the player must slide the numbered tiles (using the arrow-keys) to allow tiles with the same number to merge together. 

Each tile value is a power of 2 starting at 2 and can go as high as 2<sup>17</sup> and a new low values tile is added to the board with each move that is made.

![2048](assets/2048.jpg)

## Proposed Features
### Game Board
An interactive 4x4 game board to play the game on that has displays for the players' current score and an ability to start a new game as necessary

### Gameplay Mechanics
The ability for the TUI application to read arrow key input and perform the required actions of merging or moving game tiles on the board as necessary. This will also include logic that determines when a game is over if there is no space to place a new number tile after a move is made.

### Leaderboard
A reach goal will be some form of leaderboard system where players will be able to input their names to store their scores to a long running leaderboard. This is not a priority feature.

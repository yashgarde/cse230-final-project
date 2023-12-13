{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use all" #-}


module Game (game) where

import Graphics.Vty
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style (unicodeRounded, unicodeBold)
import Data.List (transpose, nub)
import System.Random
import GHC.IO (unsafePerformIO)
import System.Random



emptyBoard :: [[Int]]
emptyBoard = [[0, 0, 0, 0],
              [0, 0, 0, 0],
              [0, 0, 0, 0],
              [0, 0, 0, 0]]

game :: IO ()
game = do
    initState <- buildInitState
    finalState <- defaultMain gameApp initState
    print finalState


data GameState =
    GameState {
        board :: [[Int]],
        score :: Int,
        currentState :: String,
        bombs :: Int,
        bombsInput :: String
    }
    deriving (Show, Eq)


data ResName =
    ResName
    deriving (Show, Eq, Ord)


gameApp :: App GameState e ResName
gameApp =
  App
    { appDraw = drawGame
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleGameEvent
    , appStartEvent = return ()
    , appAttrMap = const $ attrMap defAttr []
    }


-- UI drawing functions
buildInitState :: IO GameState
buildInitState = do
    let randomBoard = generateRandomBoard (unsafePerformIO getStdGen)
    return GameState {board = randomBoard, score = 0, currentState = "startSplash", bombs = 2, bombsInput = ""}

generateRandomBoard :: StdGen -> [[Int]]
generateRandomBoard gen = do
                          let randomIndices = randomRs (0, 15) gen :: [Int]
                              possibleIndices = [0..15]
                              selectedIndices = take 2 $ nub randomIndices
                              boardVals = map (\x -> if x `elem` selectedIndices then 2 else 0) possibleIndices
                              finalBoard = [take 4 boardVals, take 4 $ drop 4 boardVals, take 4 $ drop 8 boardVals, take 4 $ drop 12 boardVals]
                          finalBoard

generateRandomTile :: StdGen -> Int
generateRandomTile gen = do
                          let vals = randomRs (0, 9) gen :: [Int]
                              selectedVal = take 1 $ nub vals
                              v = if selectedVal!!0 == 9 then 4 else 2
                          v



drawGame :: GameState -> [Widget ResName]
drawGame state = case currentState state of
    "startSplash" -> drawStartSplash state
    "game" -> drawBoard state
    "bombsPage" -> drawBombsPage state
    "gameOver" -> drawGameOver state

drawStartSplash :: GameState -> [Widget ResName]
drawStartSplash _ = [
    hCenter $ vLimit 40 $ hLimit 40 $ withBorderStyle unicodeBold $ borderWithLabel (str "Welcome to 2048+") 
    $ padTop (Pad 1) (hCenter $ str "Press the \"S\" key to start")
    ]

drawGameOver :: GameState -> [Widget ResName]
drawGameOver state = [
    hCenter $ vLimit 40 $ hLimit 40 $ withBorderStyle unicodeBold $ border $ padTopBottom 1 (vBox $ gameOverBlock state)
    ]

gameOverBlock :: GameState -> [Widget ResName]
gameOverBlock state = [hCenter $ str "GAME OVER!", hCenter $ str $ "Final Score: " ++ show (score state), hCenter 
                        $ str "Press the \"R\" key to restart", padTop (Pad 2) $ hCenter $ str "Press the \"Q\" key to quit"]

drawBombsPage :: GameState -> [Widget ResName]
drawBombsPage state = [
    hCenter $ vLimit 60 $ hLimit 100 $ withBorderStyle unicodeBold $ border $ padTopBottom 1 (vBox $ bombUseBlock state)
    ]

bombUseBlock :: GameState -> [Widget ResName]
bombUseBlock state = [hCenter $ str $ "You have " ++ show (bombs state) ++ " bombs left", 
                        hCenter $ str $ "Please enter a number (upto 32) that you wish to clear and press Enter: " ++ bombsInput state, 
                        hCenter $ str "Press ESC to return to the game"]

-- TODO: display score on board

drawBoard :: GameState -> [Widget ResName]
drawBoard state = [
    hCenter $ vLimit 100 $ hLimit 50 $ withBorderStyle unicodeRounded $ borderWithLabel (str "2048") $
        vBox (map (hCenter . drawBoardRow) (board state)) <=>
        padTopBottom 1 (hCenter $ str $ "Bombs left: " ++ show (bombs state)) <=>
        padTop (Pad 2) (hCenter $ str $ "CURRENT SCORE: " ++ show (score state))
    ]

drawBoardRow :: [Int] -> Widget ResName
drawBoardRow row = hBox $ map drawCell row

drawCell :: Int -> Widget ResName
drawCell val =
    let cellDisp = if val == 0 then " " else show val in
    hLimit 10 $ withBorderStyle unicodeRounded $ border $ hCenter $ padAll 1 $ str cellDisp



-- TODO: Place all Logic functions here or in a separate file as needed


-- also add the random cell onto the board


shiftRows :: [[Int]] -> [[Int]]
shiftRows board = map shiftRow board
  where
      shiftRow :: [Int] -> [Int]
      shiftRow row = (filter (/= 0) row) ++ (replicate (numZeros row) 0)
        where
          numZeros :: [Int] -> Int
          numZeros r = length (filter (== 0) r)

reverseRows :: [[Int]] -> [[Int]]
reverseRows = map reverse

listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + (listSum xs)


-- merging should be directional as well
mergeRows :: [[Int]] -> ([[Int]], Int)
mergeRows board = (newBoard, score)
  where
    mergeRow :: Int -> [Int] -> ([Int], Int)
    mergeRow 0 _ = ([], 0)
    mergeRow n [] = (replicate n 0, 0)
    mergeRow n [x] = ([x] ++ (replicate (n - 1) 0), 0)
    mergeRow n (x:y:xs) 
      | x == y = ((x + y) : eq_b, x + y + eq_s)
      | otherwise = (x : neq_b, neq_s)
        where
          (eq_b, eq_s) = (mergeRow (n - 1) xs)
          (neq_b, neq_s) = (mergeRow (n - 1) (y:xs))
    mergedOutput = (map (mergeRow 4) board)
    score = listSum (map snd mergedOutput)
    newBoard = (map fst mergedOutput)

shiftDown :: [[Int]] -> ([[Int]], Int)
shiftDown board = (transpose (reverseRows b), score)
    where
      (b, score) = mergeRows (shiftRows (reverseRows (transpose board)))

shiftUp :: [[Int]] -> ([[Int]], Int)
shiftUp board = (transpose b, score)
    where
      (b, score) = mergeRows (shiftRows (transpose board))

shiftLeft :: [[Int]] -> ([[Int]], Int)
shiftLeft board = mergeRows (shiftRows board)

shiftRight :: [[Int]] -> ([[Int]], Int)
shiftRight board = (reverseRows b, score)
    where
      (b, score) = mergeRows (shiftRows (reverseRows board))



-- try out newStdGen and getStdGen. Check to see if there is a difference
-- add the new random tile value to the score? - i dont think so

-- work in progress: generating random tile
-- randNum = randomIO :: IO Int

addTile :: [[Int]] -> [[Int]]
addTile board = board
  -- where
  --   tileVal = newTileVal

-- randomNums :: Int -> [Double]
-- randomNums seed = randoms (mkStdGen seed) :: [Double]

-- newTileVal :: Int
-- newTileVal = generateRandomTile (unsafePerformIO getStdGen)

-- generateRandomBoard :: StdGen -> [[Int]]
-- generateRandomBoard gen = do
--                           let randomIndices = randomRs (0, 15) gen :: [Int]
--                               possibleIndices = [0..15]
--                               selectedIndices = take 2 $ nub randomIndices
--                               boardVals = map (\x -> if x `elem` selectedIndices then 2 else 0) possibleIndices
--                               finalBoard = [take 4 boardVals, take 4 $ drop 4 boardVals, take 4 $ drop 8 boardVals, take 4 $ drop 12 boardVals]
--                           finalBoard

-- TODO: Add checks to ensure the board is not completely emptied out
removeFromBoard :: Int -> [[Int]] -> ([[Int]], Bool)
removeFromBoard n gameBoard = 
    let removeCellFromBoard row = map (\x -> if x == n then 0 else x) row
        bombedBoard = map removeCellFromBoard gameBoard
        isEmpty = bombedBoard == emptyBoard
    in if isEmpty then (gameBoard, isEmpty) else (bombedBoard, isEmpty)


-- Event handling function: TODO: clean this up to use case or better pattern matching
keyPress :: Char -> GameState -> GameState
keyPress 'x' g = GameState {board = newB, score = s + (score g), currentState = currentState g, bombs = bombs g, bombsInput = bombsInput g}
    where
      (b, s) = shiftDown (board g)
      newB = addTile b
keyPress 'd' g = GameState {board = newB, score = s + (score g), currentState = currentState g, bombs = bombs g, bombsInput = bombsInput g}
    where
      (b, s) = shiftUp (board g)
      newB = addTile b
keyPress 'z' g = GameState {board = newB, score = s + (score g), currentState = currentState g, bombs = bombs g, bombsInput = bombsInput g}
    where
      (b, s) = shiftLeft (board g)
      newB = addTile b
keyPress 'c' g = GameState {board = newB, score = s + (score g), currentState = currentState g, bombs = bombs g, bombsInput = bombsInput g}
    where
      (b, s) = shiftRight (board g)
      newB = addTile b

keyPress 's' g = if currentState g == "startSplash" then GameState {board = board g, score = score g, currentState = "game", bombs = bombs g, bombsInput = bombsInput g} else g
-- Currently only generates a new random board on the first reset but the same board from there on out
keyPress 'r' g = let newBoard = generateRandomBoard (unsafePerformIO newStdGen) in
    GameState {board = newBoard, score = 0, currentState = "game", bombs = bombs g, bombsInput = bombsInput g}

-- FOR TESTING PURPOSES ONLY
keyPress 'g' g = GameState {board = board g, score = score g, currentState = "gameOver", bombs = bombs g, bombsInput = bombsInput g}

-- TODO: Possible bomb feature with a Dialog or a Brick Form
keyPress 'b' g = if currentState g == "game" then 
                    GameState {board = board g, score = score g, currentState = "bombsPage", bombs = bombs g, bombsInput = bombsInput g} else g

keyPress 'e' g = if currentState g == "bombsPage" then GameState {board = board g, score = score g, currentState = "game", bombs = bombs g, bombsInput = ""} else g

keyPress 'n' g = let (bombedBoard, isEmpty) = removeFromBoard (read $ bombsInput g) (board g) in
                    if currentState g == "bombsPage" && (read (bombsInput g) :: Int) <= 32 && not isEmpty then 
                        GameState {board = bombedBoard , score = score g, currentState = "game", bombs = bombs g - 1, bombsInput = ""} 
                        else GameState {board = bombedBoard , score = score g, currentState = "game", bombs = bombs g, bombsInput = ""}

keyPress key g
    | currentState g == "bombsPage" && key `elem` ['0'..'9'] = GameState {board = board g, score = score g, currentState = "bombsPage", bombs = bombs g, bombsInput = bombsInput g ++ [key]}
    | otherwise = g




handleGameEvent :: BrickEvent ResName e -> EventM ResName GameState ()
handleGameEvent e =
    case e of
        VtyEvent vte ->
            case vte of
                EvKey (KChar 'q') [] -> halt
                EvKey (KChar 'd') [] -> modify $ keyPress 'd'
                EvKey (KChar 's') [] -> modify $ keyPress 's'
                EvKey (KChar 'r') [] -> modify $ keyPress 'r'
                EvKey (KChar 'g') [] -> modify $ keyPress 'g'
                EvKey (KChar 'x') [] -> modify $ keyPress 'x'
                EvKey (KChar 'z') [] -> modify $ keyPress 'z'
                EvKey (KChar 'c') [] -> modify $ keyPress 'c'

                -- Bomb events
                EvKey (KChar 'b') [] -> modify $ keyPress 'b'
                EvKey KEsc []        -> modify $ keyPress 'e'
                EvKey KEnter []      -> modify $ keyPress 'n'
                EvKey (KChar key) [] | key `elem` ['0'..'9'] -> modify $ keyPress key
                _ -> continueWithoutRedraw
        _ -> continueWithoutRedraw
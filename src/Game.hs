{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Game where

import Graphics.Vty
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style (unicodeRounded, unicodeBold)
import Data.List (transpose, nub)
import System.Random
import GHC.IO (unsafePerformIO)


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
        bombs :: Int
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
    return GameState {board = randomBoard, score = 0, currentState = "startSplash", bombs = 2}

generateRandomBoard :: StdGen -> [[Int]]
generateRandomBoard gen = do
                          let randomIndices = randomRs (0, 15) gen :: [Int]
                              possibleIndices = [0..15]
                              selectedIndices = take 2 $ nub randomIndices
                              boardVals = map (\x -> if x `elem` selectedIndices then 2 else 0) possibleIndices
                              finalBoard = [take 4 boardVals, take 4 $ drop 4 boardVals, take 4 $ drop 8 boardVals, take 4 $ drop 12 boardVals]
                          finalBoard



drawGame :: GameState -> [Widget ResName]
drawGame state = case currentState state of
    "startSplash" -> drawStartSplash state
    "game" -> drawBoard state
    "gameOver" -> drawGameOver state

drawStartSplash :: GameState -> [Widget ResName]
drawStartSplash _ = [
    hCenter $ vLimit 40 $ hLimit 40 $ withBorderStyle unicodeBold $ borderWithLabel (str "Welcome to 2048+") $ padTop (Pad 1) (hCenter $ str "Press the \"S\" key to start")
    ]

drawGameOver :: GameState -> [Widget ResName]
drawGameOver state = [
    hCenter $ vLimit 40 $ hLimit 40 $ withBorderStyle unicodeBold $ border $ padTopBottom 1 (vBox $ gameOverBlock state)
    ]

gameOverBlock :: GameState -> [Widget ResName]
gameOverBlock state = [hCenter $ str "GAME OVER!", hCenter $ str $ "Final Score: " ++ show (score state), hCenter $ str "Press the \"R\" key to restart", hCenter $ str "Press the \"Q\" key to quit"]


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


countZeros :: (Num a, Eq a) => [a] -> Int
countZeros = length . filter (== 0)

moveNumsToBottom :: [[Int]] -> [[Int]]
moveNumsToBottom gameBoard =
  let moveColumnToBottom col = replicate (length col - countZeros col) 0 ++ filter (/= 0) col
      transposedGrid = transpose gameBoard
  in transpose (map moveColumnToBottom transposedGrid)




-- Event handling function
keyPress :: Char -> GameState -> GameState
keyPress 'd' g = GameState {board = moveNumsToBottom (board g), score = 0, currentState = currentState g, bombs = bombs g}
keyPress 's' g = if currentState g == "startSplash" then GameState {board = board g, score = score g, currentState = "game", bombs = bombs g} else g
-- Currently only generates a new random board on the first reset but the same board from there on out
keyPress 'r' g = let newBoard = generateRandomBoard (unsafePerformIO newStdGen) in
    GameState {board = newBoard, score = 0, currentState = "game", bombs = bombs g}
-- FOR TESTING PURPOSES ONLY
keyPress 'g' g = GameState {board = board g, score = score g, currentState = "gameOver", bombs = bombs g}

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
                _ -> continueWithoutRedraw
        _ -> continueWithoutRedraw
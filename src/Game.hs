{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use all" #-}


module Game (game) where

import qualified GameLogic as L
import Graphics.Vty
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style (unicodeRounded, unicodeBold)
import System.Random ( randomIO, mkStdGen, Random(randoms) )



game :: IO ()
game = do
    num <- randomIO
    initState <- buildInitState num
    _ <- defaultMain gameApp initState
    print ("game over" :: String)


data GameState =
    GameState {
        board :: [[Int]],
        score :: Int,
        currentState :: String,
        bombs :: Int,
        bombsInput :: String,
        randNums :: [Double]
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
buildInitState :: Int -> IO GameState
buildInitState seed = do
    let randomNums = randoms (mkStdGen seed)
    let vals = take 2 randomNums
    let randomBoard = L.generateRandomBoard vals
    return GameState {board = randomBoard, score = 0, currentState = "startSplash", bombs = 2, bombsInput = "", randNums = drop 2 randomNums}


-- Brick drawing functions
drawGame :: GameState -> [Widget ResName]
drawGame state = case currentState state of
    "startSplash" -> drawStartSplash state
    "controls" -> drawControls state
    "game" -> drawBoard state
    "bombsPage" -> drawBombsPage state
    "gameOver" -> drawGameOver state

drawStartSplash :: GameState -> [Widget ResName]
drawStartSplash _ = [
    hCenter $ vLimit 40 $ hLimit 40 $ withBorderStyle unicodeBold $ borderWithLabel (padLeftRight 1 $ str "Welcome to 2048+")
    $ padTop (Pad 1) (vBox [hCenter $ padLeftRight 1 $ str "Press the \"S\" key to start", hCenter $ padAll 1 $ str "Press the \"C\" key to view controls"])
    ]

drawControls :: GameState -> [Widget ResName]
drawControls _ = [
    hCenter $ vLimit 60 $ hLimit 100 $ withBorderStyle unicodeBold $ borderWithLabel (padLeftRight 1 $ str "2048+ Controls") $ padAll 1 (vBox controlsBlock)
    ]

controlsBlock :: [Widget ResName]
controlsBlock = [hCenter $ str "Use the arrow keys (← → ↑ ↓) to slide the tiles in the respective directions",
                    hCenter $ str "Press the \"B\" key for the bomb 💣 powerup",
                    hCenter $ str "Press the \"R\" key to restart the game",
                    hCenter $ str "Press the \"ESC\" key to return to the game"]

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
bombUseBlock state = [hCenter $ str $ "You have " ++ show (bombs state) ++ " 💣 left",
                        hCenter $ str $ "Please enter a number (upto 32) that you wish to clear and press Enter: " ++ bombsInput state,
                        hCenter $ str "Press ESC to return to the game"]


drawBoard :: GameState -> [Widget ResName]
drawBoard state = [
    hCenter $ vLimit 100 $ hLimit 50 $ withBorderStyle unicodeRounded $ borderWithLabel (padLeftRight 1 $ str "2048+") $
        vBox (map (hCenter . drawBoardRow) (board state)) <=>
        padTopBottom 1 (hCenter $ str $ "💣 left: " ++ show (bombs state)) <=>
        padTop (Pad 2) (hCenter $ str $ "CURRENT SCORE: " ++ show (score state))
    ]

drawBoardRow :: [Int] -> Widget ResName
drawBoardRow row = hBox $ map drawCell row

drawCell :: Int -> Widget ResName
drawCell val =
    let cellDisp = if val == 0 then " " else show val in
    hLimit 10 $ withBorderStyle unicodeRounded $ border $ hCenter $ padAll 1 $ str cellDisp


-- Shravan's TODOs: Random tile placement and ending the game once all positions are filled. fix the issue with dissappearing tiles. add random nums list to the game state. change the keys for press. add the new powerups (swap two tiles, don't add a new tile). refactor the code. bomb valid inputs. bomb number cant go below 0
-- dont add new tile if a move resulted in no tiles changing

-- Event handling function: TODO: clean this up to use case or better pattern matching
-- need to handle the case where the entire board fills up; need to end game
keyPress :: Char -> GameState -> GameState
keyPress 'x' g = shiftAndAddTile L.shiftDown g
keyPress 'd' g = shiftAndAddTile L.shiftUp g
keyPress 'z' g = shiftAndAddTile L.shiftLeft g
keyPress 'm' g = shiftAndAddTile L.shiftRight g

keyPress 's' g
    | currentState g == "startSplash" = g { currentState = "game" }
    | otherwise = g

keyPress 'r' g = g { board = newBoard, score = 0, currentState = "game", bombsInput = "", randNums = upNums }
    where
        newBoard = L.generateRandomBoard (take 2 (randNums g))
        upNums = drop 2 (randNums g)

keyPress 'c' g
    | currentState g == "startSplash" = g { currentState = "controls" }
    | otherwise = g

keyPress 'g' g = g { currentState = "gameOver" }

keyPress 'b' g
    | currentState g == "game" && (bombs g) > 0 = g { currentState = "bombsPage" }
    | otherwise = g

keyPress 'e' g
    | currentState g == "bombsPage" = g { currentState = "game", bombsInput = "" }
    | currentState g == "controls"  = g { currentState = "startSplash" }
    | otherwise = g

keyPress 'n' g
    | currentState g == "bombsPage" && (read (bombsInput g) :: Int) <= 32 && not isEmpty = if (bombs g - 1) == 0 && (L.isGameOver (board g) 0) then g { currentState = "gameOver" }
        else g { board = bombedBoard, currentState = "game", bombs = bombs g - 1, bombsInput = "" }
    | otherwise = g { currentState = "game", bombsInput = "" }
    where
        (bombedBoard, isEmpty) = L.removeFromBoard (read (bombsInput g) :: Int) (board g)

keyPress key g
    | currentState g == "bombsPage" && key `elem` ['0'..'9'] =
        g { currentState = "bombsPage", bombsInput = bombsInput g ++ [key] }
    | otherwise = g

shiftAndAddTile :: ([[Int]] -> ([[Int]], Int)) -> GameState -> GameState
shiftAndAddTile shiftFn g =
    if currentState g == "game" then
        let (b, s) = shiftFn (board g)
            newB = if L.flatten (board g) /= L.flatten b then L.addTile b (take 2 (randNums g)) else b
            upNums = drop 2 (randNums g)
        in if L.isGameOver newB (bombs g) then
            GameState {board = newB, score = s + score g, currentState = "gameOver", bombs = bombs g, bombsInput = bombsInput g, randNums = upNums}
            else GameState {board = newB, score = s + score g, currentState = currentState g, bombs = bombs g, bombsInput = bombsInput g, randNums = upNums}
    else g



handleGameEvent :: BrickEvent ResName e -> EventM ResName GameState ()
handleGameEvent e =
    case e of
        VtyEvent vte ->
            case vte of
                EvKey (KChar 'q') [] -> halt
                EvKey (KChar 's') [] -> modify $ keyPress 's' -- Game start
                EvKey (KChar 'r') [] -> modify $ keyPress 'r' -- Game reset
                EvKey (KChar 'c') [] -> modify $ keyPress 'c' -- Controls page

                -- Only for testing game over
                EvKey (KChar 'g') [] -> modify $ keyPress 'g'

                -- Game sliding
                EvKey KUp [] -> modify $ keyPress 'd'
                EvKey KDown [] -> modify $ keyPress 'x'
                EvKey KLeft [] -> modify $ keyPress 'z'
                EvKey KRight [] -> modify $ keyPress 'm'

                -- Bomb events
                EvKey (KChar 'b') [] -> modify $ keyPress 'b'
                EvKey KEsc []        -> modify $ keyPress 'e'
                EvKey KEnter []      -> modify $ keyPress 'n'
                EvKey (KChar key) [] | key `elem` ['0'..'9'] -> modify $ keyPress key


                _ -> continueWithoutRedraw
        _ -> continueWithoutRedraw
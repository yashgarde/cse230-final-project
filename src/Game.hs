{-# LANGUAGE OverloadedStrings #-}


module Game where

import Graphics.Vty
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style (unicodeRounded)

import Data.List (transpose)

import Control.Monad (void)
import Control.Monad.State (get, put, modify)

-- import Lens.Micro (_1)
-- import Lens.Micro.Mtl (use, (.=))

game :: IO ()
game = do
    initState <- buildInitState
    finalState <- defaultMain gameApp initState
    print finalState


data GameState =
    GameState {
        board :: [[Int]],
        score :: Int
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

buildInitState :: IO GameState
-- Currently just returns a hardcoded board -- TODO: Make this random
buildInitState = return GameState {board = [[0,0,0,0],[0,2,0,0],[0,0,2,0],[0,0,0,0]], score = 0}

drawGame :: GameState -> [Widget ResName]
drawGame state = [
    withBorderStyle unicodeRounded $ borderWithLabel (str "2048") $ (vBox $ map (hCenter . drawBoardRow) (board state)) <=> (padTop (Pad 1) $ hCenter $ str $ "Current Score: " ++ show (score state))
    ]

drawBoardRow :: [Int] -> Widget ResName
drawBoardRow row = hBox $ map drawCell row

drawCell :: Int -> Widget ResName
drawCell val = 
    let cellDisp = if val == 0 then " " else show val in
    hLimit 10 $ withBorderStyle unicodeRounded $ border $ hCenter $ padAll 1 $ str cellDisp

-- also add the random cell onto the board


countZeros :: (Num a, Eq a) => [a] -> Int
countZeros = length . filter (== 0)

moveNumsToBottom :: [[Int]] -> [[Int]]
moveNumsToBottom board =
  let moveColumnToBottom col = replicate (length col - countZeros col) 0 ++ filter (/= 0) col
      transposedGrid = transpose board
  in transpose (map moveColumnToBottom transposedGrid)


-- keyPress :: Char -> EventM n2 s ()
keyPress 'd' = do
    old_state <- get
    let new_state = GameState {board = (moveNumsToBottom (board old_state)), score = 0}
    put new_state

handleGameEvent :: BrickEvent n1 e -> EventM n2 s ()
handleGameEvent e =
    case e of
        VtyEvent vte ->
            case vte of
                EvKey (KChar 'q') [] -> halt
                EvKey (KChar 'd') [] -> keyPress 'd'
                _ -> continueWithoutRedraw
        _ -> continueWithoutRedraw
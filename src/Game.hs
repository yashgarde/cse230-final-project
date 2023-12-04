{-# LANGUAGE OverloadedStrings #-}


module Game where

import Graphics.Vty
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Border.Style (unicodeRounded)

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

handleGameEvent :: BrickEvent n1 e -> EventM n2 s ()
handleGameEvent e =
    case e of
        VtyEvent vte ->
            case vte of
                EvKey (KChar 'q') [] -> halt
                _ -> continueWithoutRedraw
        _ -> continueWithoutRedraw
{-# LANGUAGE OverloadedStrings #-}


module Game where

import Graphics.Vty
import Brick
import Brick.Widgets.Core
import Brick.Main
import Brick.Types
import Brick.AttrMap

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
buildInitState = return GameState {board = [[]]}

drawGame :: GameState -> [Widget ResName]
drawGame state = []


handleGameEvent e =
    case e of
        VtyEvent vte ->
            case vte of
                EvKey (KChar 'q') [] -> halt
                _ -> continueWithoutRedraw
        _ -> continueWithoutRedraw
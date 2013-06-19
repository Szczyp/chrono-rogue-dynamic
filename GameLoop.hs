module GameLoop (gameLoop) where

import Systems
import Systems.Collide
import Systems.Draw
import Types

import Control.Arrow
import System.Console.ANSI

gameLoop :: Level -> IO ()
gameLoop level = do
    putStr . draw $ level
    direction <- input
    clearScreen
    let cleanLevel = clearLevelInfo level
    move direction >>> collide cleanLevel >>> gameLoop $ cleanLevel

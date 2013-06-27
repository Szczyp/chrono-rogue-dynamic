module GameLoop (gameLoop) where

import Systems
import Systems.Collide
import Systems.Draw
import Systems.Memorize
import Types
import Utils

import Data.UUID
import System.Console.ANSI

processLevel :: UUID -> Direction -> Level -> Level
processLevel eId direction = memorize
                           . step
                           . move
                           . collide
                           . withId eId (addMove direction)
                           . clearInfo

gameLoop :: UUID -> Level -> IO ()
gameLoop eId level = do
    printLevel
    printInfo
    direction <- input
    clearScreen
    gameLoop eId . processLevel eId direction $ level
  where printLevel = putStr . drawFor eId $ level
        printInfo  = putStr . collectInfo $ level

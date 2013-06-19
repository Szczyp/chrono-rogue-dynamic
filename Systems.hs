module Systems where

import Components
import Types

import Prelude hiding (Left, Right, floor, lookup, map)

import Data.Set


move :: Direction -> Level -> Level
move d = map $ \e -> if hasHero e then mapC (walk d) e else e

walk :: Direction -> Position -> Position
walk Stay      pos            = pos
walk Up        (Position x y) = Position x       (y - 1)
walk UpRight   (Position x y) = Position (x + 1) (y - 1)
walk Right     (Position x y) = Position (x + 1) y
walk DownRight (Position x y) = Position (x + 1) (y + 1)
walk Down      (Position x y) = Position x       (y + 1)
walk DownLeft  (Position x y) = Position (x - 1) (y + 1)
walk Left      (Position x y) = Position (x - 1) y
walk UpLeft    (Position x y) = Position (x - 1) (y - 1)

input :: IO Direction
input = do
    char <- getChar
    return $ case char of
      's' -> Stay
      'w' -> Up
      'e' -> UpRight
      'd' -> Right
      'c' -> DownRight
      'x' -> Down
      'z' -> DownLeft
      'a' -> Left
      'q' -> UpLeft
      _   -> Stay

clearLevelInfo :: Level -> Level
clearLevelInfo = map $ mapC $ const (LevelInfo [])

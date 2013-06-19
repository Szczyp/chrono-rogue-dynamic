module Systems where

import Components
import Types
import Utils

import Prelude hiding (Left, Right)

import qualified Data.Set as S

move :: Direction -> Level -> Level
move d = S.map $ iff hasHero $ mapC (walk d)

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
clearLevelInfo = S.map $ mapC $ const (LevelInfo [])

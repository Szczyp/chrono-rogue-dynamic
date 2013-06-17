module Systems where

import Components
import Types
import Utils

import Prelude hiding (Left, Right, floor, lookup)

import Data.List hiding (lookup, filter)
import Data.Map hiding (map, null, filter, mapMaybe)
import Data.Maybe
import qualified Data.Set as S
import Control.Monad


drawLevel :: Level -> String
drawLevel level = unlines . map makeRow $ [1 .. 10]
  where makeRow y = map (sigilOrDot y) [1 .. 10]
        sigilOrDot y x = fst . fromMaybe floor . lookup (x, y) $ coordMap
        coordMap = fromList . sortByLayer . mapMaybe renderable $ level
        sortByLayer = sortBy $ \(_, (_, l)) (_, (_, l')) -> compare l l'
        floor = ('.', 0)
        renderable entity = do
            p <- pos entity
            s <- sig entity
            return (p, (s, lay entity))
        pos = convert position toCoord
        sig = convert sigil toChar
        lay = toInt . layerOr (Layer 0)

processCollision :: Level -> Level -> Level
processCollision old new = if isUnique $ mapMaybe collidable new then new else old
  where collidable entity = do
          guard $ hasCollision entity
          pos entity 
        pos = convert position toCoord
        isUnique l = length l == S.size (S.fromList l)

moveHeroes :: Direction -> Level -> Level
moveHeroes direction = map $ mapCmp hasHero $ walk direction

walk :: Direction -> Position -> Position
walk Stay      pos               = pos
walk Up        (Position (x, y)) = Position (x, y - 1)
walk UpRight   (Position (x, y)) = Position (x + 1, y - 1)
walk Right     (Position (x, y)) = Position (x + 1, y)
walk DownRight (Position (x, y)) = Position (x + 1, y + 1)
walk Down      (Position (x, y)) = Position (x, y + 1)
walk DownLeft  (Position (x, y)) = Position (x - 1, y + 1)
walk Left      (Position (x, y)) = Position (x - 1, y)
walk UpLeft    (Position (x, y)) = Position (x - 1, y - 1)

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

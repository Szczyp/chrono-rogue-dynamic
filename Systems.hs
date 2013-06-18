{-# LANGUAGE TupleSections #-}
module Systems where

import Components
import Types
import Utils

import Prelude hiding (Left, Right, floor, lookup)

import Data.Function
import Data.List hiding (lookup, filter, foldl)
import Data.Map hiding (map, null, filter, mapMaybe, foldl)
import Data.Maybe
import Control.Applicative


drawLevel :: Level -> String
drawLevel level = unlines $ map makeRow [1 .. 10] ++ info level
  where makeRow y = map (entityOrFloor y) [1 .. 10]
        entityOrFloor y x = render . fst . fromMaybe floor . lookup (Position (x, y)) $ coordMap
        coordMap = fromList . sortBy (compare `on` snd . snd) . mapMaybe renderable $ level
        floor = (Sigil '.', Layer 0)
        renderable entity =
            let triple = (,,layerOr (Layer 0) entity) <$> position entity <*> sigil entity
                rearange (p, s, l) = (p, (s, l))
            in rearange <$> triple
        info = concatMap (\(LevelInfo l) -> l) . map (levelInfoOr (LevelInfo []))


processCollision :: Level -> Level -> Level
processCollision old new = if null collisions then new else collide old
  where collidable entity = (,) <$> position entity <*> convert collision toFunc entity
        collisions = concatMap (map snd) . filter ((> 1) . length) . groupByPosition . mapMaybe collidable $ new
        groupByPosition = groupBy ((==) `on` fst) . sortBy (compare `on` fst)
        collide = foldl (.) id collisions
        toFunc (Collision f) = f

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

clearLevelInfo :: Level -> Level
clearLevelInfo = map $ mapAllCmp $ const (LevelInfo [])

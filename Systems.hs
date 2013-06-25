module Systems where

import Components
import Types
import Utils

import Prelude hiding (Left, Right)

import Data.List
import Data.Maybe
import qualified Data.Set as S

moveHero :: Direction -> Level -> Level
moveHero = S.mapMonotonic . iff hasHero . add . Move

processMove :: Level -> Level
processMove = S.mapMonotonic move'
  where move' e = fromMaybe e $ do
          (Move d) <- move e
          return $ removeMove . mapC (walk d) $ e

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

printInfo :: Level -> String
printInfo = concatMap show . reverse . mapMaybe info . S.toList

instance Show Info where
    show (Info i) = unlines i

clearInfo :: Level -> Level
clearInfo = S.mapMonotonic removeInfo

class Eq a => Interact a where
    srcF :: a -> Entity -> Entity -> Entity
    trgF :: a -> Entity -> Entity -> Entity
    entity :: a -> Entity

    interact :: [a] -> [Entity]
    interact xs = map thread xs
        where thread x = ($ entity x)
                       . foldl (.) id
                       . map (combine x)
                       . delete x
                       $ xs
              combine x a' = srcF x (entity a') . trgF a' (entity a')

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


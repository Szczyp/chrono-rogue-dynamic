module Systems where

import Components
import Types

import Prelude hiding (Left, Right)

import Control.Applicative
import Data.List (delete)
import Data.Maybe
import Data.Set (insert, mapMonotonic, toList)

addMove :: Direction -> Entity -> Level -> Level
addMove d e = insert $ add (Move d) e

move :: Level -> Level
move = mapMonotonic move'
  where move' e = fromMaybe e $ do
          (Move d) <- getMove e
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

collectInfo :: Level -> String
collectInfo = concatMap show . reverse . mapMaybe getInfo . toList

instance Show Info where
    show (Info i) = unlines i

clearInfo :: Level -> Level
clearInfo = mapMonotonic removeInfo

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
              combine x x' = srcF x (entity x') . trgF x' (entity x')

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

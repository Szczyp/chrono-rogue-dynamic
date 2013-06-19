module Systems.Collide (collide) where

import Components
import Types
import Utils

import Prelude hiding (Left, Right, floor, lookup)

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List hiding (lookup, filter, foldl)
import Data.Map hiding (map, null, filter, mapMaybe, foldl)
import Data.Maybe
import Control.Applicative

data Collidable = Collidable { cEntity   :: Entity
                             , cPosition :: Position
                             , cFunc     :: Entity -> (Entity, Entity) }


collide :: Level -> Level -> Level
collide old new = new --if null collisions then new else collide old
  -- where collidable entity = Collidable entity
  --                         <$> position entity
  --                         <*> convert collision (func entity) entity
  --       collisions = filter ((> 1) . length) . groupBy ((==) `on` cPosition)
  --                     . sortBy (compare `on` cPosition) . map collidable
  --       collide = foldl (.) id collisions
  --       func entity (Collision f) = f entity


module Systems.Collide (collide) where

import Components
import Types

import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Collidable = Collidable { cEntity    :: Entity
                             , cPosition  :: Position
                             , cCollision :: Collision }

collide :: Level -> Level -> Level
collide old new = if null . collisions $ new then new else old
  where collidable e = Collidable e
                       <$> position e
                       <*> collision e
        collisions = filter ((> 1) . length) . groupBy ((==) `on` cPosition)
                     . sortBy (compare `on` cPosition) . mapMaybe collidable
                     . S.toList

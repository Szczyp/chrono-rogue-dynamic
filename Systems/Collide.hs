module Systems.Collide (collide, step) where

import Components
import Systems
import Types

import Prelude hiding (interact)

import Control.Applicative
import Data.Function
import Data.List (groupBy, sortBy)
import Data.Maybe
import Data.Set (fromList, toList, union)

data Collidable = Collidable { collidableEntity    :: Entity
                             , collidablePosition  :: Position
                             , collidableCollision :: Collision
                             , collidableMove      :: Maybe Move }

instance Eq Collidable where
    (==) = (==) `on` collidableEntity

instance Interact Collidable where
    srcF Collidable {collidableCollision = (Collision s _)} = s
    trgF Collidable {collidableCollision = (Collision _ t)} = t
    entity = collidableEntity

collide :: Level -> Level
collide level = collidedLevel `union` level
    where collidedLevel = fromList
                        . map removeMove
                        . concatMap interact
                        $ collisions
          collisions = filter ((> 1) . length)
                     . groupBy ((==) `on` collidablePosition)
                     . sortBy (compare `on` collidablePosition)
                     . map moveC
                     . mapMaybe collidable
                     . toList
                     $ level
          moveC c = fromMaybe c $ do
            (Move d) <- collidableMove c
            return $ c {collidablePosition = walk d . collidablePosition $ c}
          collidable e = Collidable e
                         <$> getPosition e
                         <*> getCollision e
                         <*> pure (getMove e)


data Stepable = Stepable { stepableEntity   :: Entity
                         , stepablePosition :: Position
                         , stepableTile     :: Tile }

instance Eq Stepable where
    (==) = (==) `on` stepableEntity

instance Interact Stepable where
    srcF Stepable {stepableTile = (Tile s _)} = s
    trgF Stepable {stepableTile = (Tile _ t)} = t
    entity = stepableEntity

step :: Level -> Level
step level = collidedLevel `union` level
    where collidedLevel = fromList
                        . concatMap interact
                        $ collisions
          collisions = filter ((> 1) . length)
                     . groupBy ((==) `on` stepablePosition)
                     . sortBy (compare `on` stepablePosition)
                     . mapMaybe stepable
                     . toList
                     $ level
          stepable e = Stepable e
                       <$> getPosition e
                       <*> getTile e

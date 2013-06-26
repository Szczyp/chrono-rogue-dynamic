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

data Collidable = Collidable { cEntity    :: Entity
                             , cPosition  :: Position
                             , cCollision :: Collision
                             , cMove      :: Maybe Move }

instance Eq Collidable where
    (==) = (==) `on` cEntity

instance Interact Collidable where
    srcF Collidable {cCollision = (Collision s _)} = s
    trgF Collidable {cCollision = (Collision _ t)} = t
    entity = cEntity

collide :: Level -> Level
collide level = collidedLevel `union` level
    where collidedLevel = fromList
                        . map removeMove
                        . concatMap interact
                        $ collisions
          collisions = filter ((> 1) . length)
                     . groupBy ((==) `on` cPosition)
                     . sortBy (compare `on` cPosition)
                     . map moveC
                     . mapMaybe collidable
                     . toList
                     $ level
          moveC c = fromMaybe c $ do
            (Move d) <- cMove c
            return $ c {cPosition = walk d . cPosition $ c}
          collidable e = Collidable e
                         <$> position e
                         <*> collision e
                         <*> pure (move e)


data Stepable = Stepable { sEntity   :: Entity
                         , sPosition :: Position
                         , sTile     :: Tile }

instance Eq Stepable where
    (==) = (==) `on` sEntity

instance Interact Stepable where
    srcF Stepable {sTile = (Tile s _)} = s
    trgF Stepable {sTile = (Tile _ t)} = t
    entity = sEntity

step :: Level -> Level
step level = collidedLevel `union` level
    where collidedLevel = fromList
                        . concatMap interact
                        $ collisions
          collisions = filter ((> 1) . length)
                     . groupBy ((==) `on` sPosition)
                     . sortBy (compare `on` sPosition)
                     . mapMaybe stepable
                     . toList
                     $ level
          stepable e = Stepable e
                       <$> position e
                       <*> tile e

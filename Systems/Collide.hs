module Systems.Collide where

import Components
import Systems
import Types

import Prelude hiding (interact)

import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S

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
collide level = flip S.union level
              . S.fromList
              . map removeMove
              . concatMap collide'
              $ collisions
    where collide' cs = (map $ flip interact cs) cs
          collisions = filter ((> 1) . length)
                     . groupBy ((==) `on` cPosition)
                     . sortBy (compare `on` cPosition)
                     . map move'
                     . collidables
                     $ level
          move' c = fromMaybe c $ do
            (Move d) <- cMove c
            return $ c {cPosition = walk d . cPosition $ c}
          collidables = mapMaybe collidable . S.toList
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
step level = flip S.union level
           . S.fromList
           . concatMap step'
           $ collisions
    where step' cs = (map $ flip interact cs) cs
          collisions = filter ((> 1) . length)
                     . groupBy ((==) `on` sPosition)
                     . sortBy (compare `on` sPosition)
                     . mapMaybe stepable
                     . S.toList
                     $ level
          stepable e = Stepable e
                       <$> position e
                       <*> tile e

module Systems.Collide where

import Components
import Types

import Prelude hiding ((.), id)

import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Collidable = Collidable { cEntity    :: Entity
                             , cPosition  :: Position
                             , cCollision :: Collision }

instance Eq Collidable where
    (==) = (==) `on` cEntity

collide :: Level -> Level -> Level
collide old new = flip S.union new . S.fromList . concatMap collide' . collisions $ new
    where collide' cs = map (entity (fromOld  cs)) $ fromOld cs
          fromOld = intersect (collidables old)
          collisions = filter ((> 1) . length) . groupBy ((==) `on` cPosition)
                       . sortBy (compare `on` cPosition) . collidables
          collidables = mapMaybe collidable . S.toList
          collidable e = Collidable e
                         <$> position e
                         <*> collision e

entity :: [Collidable] -> Collidable -> Entity
entity cs c = ($ cEntity c) . foldl (.) id . map combine $ delete c cs
    where combine c' = srcF c (cEntity c') . trgF c' (cEntity c')
          srcF = (\(Collision s _) -> s) . cCollision
          trgF = (\(Collision _ t) -> t) . cCollision

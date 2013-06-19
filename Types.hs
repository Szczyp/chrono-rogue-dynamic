module Types where

import Control.Applicative
import Data.Dynamic
import Data.Function
import qualified Data.Set as S
import Data.UUID
import Data.UUID.V4


type Components = [Dynamic]

data Entity = Entity { uuid       :: UUID
                     , components :: Components }
                     deriving Show

instance Eq Entity where
    (==) = (==) `on` uuid

instance Ord Entity where
    compare = compare `on` uuid

identify :: Components -> IO Entity
identify cs = Entity <$> nextRandom <*> pure cs

setComponents :: Components -> Entity -> Entity
setComponents cs e = e {components = cs}

type Level = S.Set Entity

data Direction = Stay
               | Up
               | UpRight
               | Right
               | DownRight
               | Down
               | DownLeft
               | Left
               | UpLeft


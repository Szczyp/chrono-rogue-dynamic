module Types where

import Data.Dynamic
import Data.Function
import Data.Set
import Data.UUID


type Components = [Dynamic]

data Entity = Entity { uuid       :: UUID
                     , components :: Components }
                     deriving Show

instance Eq Entity where
    (==) = (==) `on` uuid

instance Ord Entity where
    compare = compare `on` uuid

setComponents :: Components -> Entity -> Entity
setComponents cs e = e {components = cs}

emptyEntity :: UUID -> Entity
emptyEntity eId = Entity eId []

type Level = Set Entity

data Direction = Stay
               | Up
               | UpRight
               | Right
               | DownRight
               | Down
               | DownLeft
               | Left
               | UpLeft
               deriving Show


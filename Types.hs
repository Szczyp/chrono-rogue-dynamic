module Types where

import Data.Dynamic


type Level = [Entity]

type Entity = [Dynamic]

type Coord = (Int, Int)

data Direction = Stay
               | Up
               | UpRight
               | Right
               | DownRight
               | Down
               | DownLeft
               | Left
               | UpLeft


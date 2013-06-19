{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Components where

import TH
import Types

import Data.Dynamic

class Typeable c => Component c where
    add :: c -> Entity -> Entity
    mapC :: (c -> c) -> Entity -> Entity
    mapC f (Entity u cs) = Entity u $ map (\c -> maybe c (toDyn . f) (fromDynamic c)) cs


data Position = Position Int Int deriving (Eq, Ord, Show, Typeable)
register ''Position

newtype Sigil = Sigil Char deriving (Show, Typeable)
register ''Sigil

newtype Layer = Layer Int deriving (Eq, Ord, Show, Typeable)
register ''Layer

data Collision = Collision (Entity -> Entity -> (Entity, Entity)) deriving Typeable
register ''Collision

data Hero = Hero deriving Typeable
register ''Hero

newtype LevelInfo = LevelInfo [String] deriving Typeable
register ''LevelInfo

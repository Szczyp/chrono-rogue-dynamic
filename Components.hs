{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Components where

import TH
import Types

import Data.Dynamic

class Typeable c => Component c where
    add :: c -> Entity -> Entity

    mapC :: (c -> c) -> Entity -> Entity
    mapC f (Entity u cs) = Entity u $ map (\c -> maybe c (toDyn . f) (fromDynamic c)) cs

    single :: c -> Components
    single c = [toDyn c]

    (</>) :: c -> Components -> Components
    (</>) c = (:) $ toDyn c
    infixr 5 </>

    (<+>) :: (Component c') => c -> c' -> Components
    (<+>) c c' = [toDyn c, toDyn c']
    infixr 5 <+>



data Position = Position Int Int deriving (Eq, Ord, Show, Typeable)
register ''Position

newtype Sigil = Sigil Char deriving (Show, Typeable)
register ''Sigil

newtype Layer = Layer Int deriving (Eq, Ord, Show, Typeable)
register ''Layer

data Collision = Collision (Entity -> Entity -> Entity)
                           (Entity -> Entity -> Entity)
                           deriving Typeable
register ''Collision

data Hero = Hero deriving Typeable
register ''Hero

newtype Info = Info [String] deriving Typeable
register ''Info

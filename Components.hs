{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Components where

import TH
import Types
import Utils

import Data.Dynamic
import Data.Maybe
import Data.Monoid


getCmp :: Typeable c => Entity -> Maybe c
getCmp = getFirst . mconcat . map (First . fromDynamic)

getCmpOr :: Typeable c => c -> Entity -> c
getCmpOr c = fromMaybe c . getCmp

mapCmp :: Typeable c => (Entity -> Bool) -> (c -> c) -> Entity -> Entity
mapCmp p f = iff p f'
    where f' = map $ \c -> maybe c (toDyn . f) (fromDynamic c)

mapAllCmp :: Typeable c => (c -> c) -> Entity -> Entity
mapAllCmp = mapCmp $ const True


newtype Position = Position Coord deriving (Show, Typeable)

makeComponent ''Position

toCoord :: Position -> Coord
toCoord (Position c) = c


newtype Sigil = Sigil Char deriving (Show, Typeable)

makeComponent ''Sigil

toChar :: Sigil -> Char
toChar (Sigil c) = c


data Hero = Hero deriving (Show, Typeable)
makeComponent ''Hero


newtype Layer = Layer Int deriving (Show, Typeable)

makeComponent ''Layer

toInt :: Layer -> Int
toInt (Layer i) = i

data Collision = Collision deriving (Show, Typeable)

makeComponent ''Collision



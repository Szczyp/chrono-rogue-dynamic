{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Components where

import TH
import Types

import Data.Dynamic
import Data.Maybe
import Data.Set (union)

class Typeable c => Component c where
    add :: c -> Entity -> Entity

    mapC :: (c -> c) -> Entity -> Entity
    mapC f (Entity u cs) = Entity u $ map (\ c -> maybe c (toDyn . f) (fromDynamic c)) cs

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

newtype Move = Move Direction deriving (Show, Typeable)
register ''Move

newtype Sigil = Sigil Char deriving (Eq, Ord, Show, Typeable)
register ''Sigil

newtype Layer = Layer Int deriving (Eq, Ord, Show, Typeable)
register ''Layer

data Collision = Collision (Entity -> Entity -> Entity)
                           (Entity -> Entity -> Entity)
                           deriving Typeable
register ''Collision

data Tile = Tile (Entity -> Entity -> Entity)
                 (Entity -> Entity -> Entity)
                 deriving Typeable
register ''Tile

newtype Info = Info [String] deriving Typeable
register ''Info

newtype Sight = Sight Int deriving (Eq, Ord, Show, Typeable)
register ''Sight

data Opaque = Opaque deriving Typeable
register ''Opaque

newtype Memories = Memories Level deriving Typeable
register ''Memories

data Memorizable = Memorizable deriving Typeable
register ''Memorizable

addInfo :: String -> Entity -> Entity -> Entity
addInfo msg _ e = fromMaybe (add (Info [msg]) e) $ do
    (Info msgs) <- getInfo e
    return $ add (Info $ msg : msgs) e

nothing :: Entity -> Entity -> Entity
nothing = const id

unionMemories :: Memories -> Memories -> Memories
unionMemories (Memories s) (Memories s') = Memories (s `union` s')

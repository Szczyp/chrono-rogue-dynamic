module Main where

import Components hiding (hero)
import Systems
import Types

import Prelude hiding (Left, Right)

import Control.Monad
import Data.Dynamic
import Data.List
import System.IO

entity :: Entity
entity = []

(<+>) :: (Typeable a) => a -> Entity -> Entity
(<+>) x = (:) $ toDyn x
infixr 5 <+>

hero :: Entity
hero = Position (2, 2) <+>
       Sigil '@'       <+>
       Hero            <+>
       Collision id    <+>
       Layer 1         <+> entity

monster :: Entity
monster = Sigil 'k'       <+>
          Position (7, 7) <+> entity

item :: Entity
item = Sigil '$'       <+>
       Position (9, 8) <+> entity

walls :: Level
walls = do
    x <- [1 .. 10]
    y <- [1 .. 10]
    guard . not . null . intersect [1, 10] $ [x, y]
    return $ Position (x, y)                                                           <+>
             Collision (map $ mapAllCmp $ \(LevelInfo xs) -> LevelInfo ("ouch!" : xs)) <+>
             Sigil '#'                                                                 <+> entity

info :: Entity
info = LevelInfo [] <+> entity

defaultLevel :: Level
defaultLevel = [hero, monster, item, info] ++ walls

gameLoop :: Level -> IO ()
gameLoop level = do
    putStr . drawLevel $ level
    direction <- input
    let cleanLevel = clearLevelInfo level
    gameLoop . processCollision cleanLevel . moveHeroes direction $ cleanLevel

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          gameLoop defaultLevel


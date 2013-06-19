module Main where

import Components hiding (hero)
import Systems
import Systems.Collide
import Systems.Draw
import Types

import Prelude hiding (Left, Right)

import Control.Monad
import Data.Dynamic
import Data.List
import qualified Data.Set as S
import System.IO

(<+>) :: (Typeable a) => a -> Components -> Components
(<+>) x = (:) $ toDyn x
infixr 5 <+>

hero :: Components
hero = Position 2 2                   <+>
       Sigil '@'                      <+>
       Collision (\c1 c2 -> (c1, c2)) <+>
       Hero                           <+>
       Layer 1                        <+> []

monster :: Components
monster = Sigil 'k'    <+>
          Position 7 7 <+> []

item :: Components
item = Sigil '$'    <+>
       Position 9 8 <+> []

walls :: [Components]
walls = do
    x <- [1 .. 10]
    y <- [1 .. 10]
    guard . not . null . intersect [1, 10] $ [x, y]
    return $ Position x y                   <+>
             Collision (\c1 c2 -> (c1, c2)) <+>
             Sigil '#'                      <+> []

info :: Components
info = LevelInfo [] <+> []

defaultLevel :: [Components]
defaultLevel = [hero, monster, item, info] ++ walls

gameLoop :: Level -> IO ()
gameLoop level = do
    putStr . draw $ level
    direction <- input
    let cleanLevel = clearLevelInfo level
    gameLoop . collide cleanLevel . move direction $ cleanLevel

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    gameLoop . S.fromList =<< mapM identify defaultLevel


{-# LANGUAGE PostfixOperators #-}

module Main where

import Components hiding (hero)
import GameLoop
import Types

import Control.Monad
import Data.List
import System.IO
import qualified Data.Set as S

hero :: Components
hero = Position 2 2                   </>
       Sigil '@'                      </>
       Collision (\c1 c2 -> (c1, c2)) </>
       Hero                           <+>
       Layer 1

monster :: Components
monster = Sigil 'k'    <+>
          Position 7 7

item :: Components
item = Sigil '$'    <+>
       Position 9 8

walls :: [Components]
walls = do
    x <- [1 .. 10]
    y <- [1 .. 10]
    guard . not . null . intersect [1, 10] $ [x, y]
    return $ Position x y                   </>
             Collision (\c1 c2 -> (c1, c2)) <+>
             Sigil '#'

info :: Components
info = single $ LevelInfo []

defaultLevel :: [Components]
defaultLevel = [hero, monster, item, info] ++ walls


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    gameLoop . S.fromList =<< mapM identify defaultLevel


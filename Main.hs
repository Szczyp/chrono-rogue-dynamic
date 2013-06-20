module Main where

import Components hiding (hero)
import GameLoop
import Systems
import Types

import Prelude hiding (Left)

import Control.Monad
import Data.List
import System.IO
import qualified Data.Set as S

hero :: Components
hero = Position 2 2                    </>
       Sigil '@'                       </>
       Collision (const id) (const id) </>
       Hero                            <+>
       Layer 1

monster :: Components
monster = Sigil 'k'                                                 </>
          Collision (\_ -> mapC (walk Up)) (\_ -> mapC (walk Left)) <+>
          Position 7 7

item :: Components
item = Sigil '$'    <+>
       Position 9 8

walls :: [Components]
walls = do
    x <- [1 .. 10]
    y <- [1 .. 10]
    guard . not . null . intersect [1, 10] $ [x, y]
    return $ Position x y                                      </>
             Collision (const id) (\_ -> add $ Info ["ouch!"]) <+>
             Sigil '#'

defaultLevel :: [Components]
defaultLevel = [hero, monster, item] ++ walls


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    gameLoop . S.fromList =<< mapM identify defaultLevel


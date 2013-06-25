module Main where

import Components hiding (hero)
import GameLoop
import Types

import Prelude hiding (Left)

import Control.Monad
import Data.List
import System.IO
import qualified Data.Set as S

hero :: Components
hero = Position 2 2                            </>
       Sigil '@'                               </>
       Collision (addInfo "poking...") nothing </>
       Tile nothing nothing                    </>
       Hero                                    <+>
       Layer 1

monster :: Components
monster = Sigil 'k'                                                           </>
          Collision (addInfo "kobold screams: 'you shall not pass, mortal!'")
                    (addInfo "you poked a giant kobold!")                     <+>
          Position 7 7

item :: Components
item = Sigil '$'                                        </>
       Position 9 8                                     <+>
       Tile nothing (addInfo "you see plenty of gold!")

walls :: [Components]
walls = do
    x <- [1 .. 10]
    y <- [1 .. 10]
    guard . not . null . intersect [1, 10] $ [x, y]
    return $ Position x y                                                                                     </>
             Collision nothing (addInfo "you touched a cold stone wall, your finger is chilled to the bone!") <+>
             Sigil '#'

defaultLevel :: [Components]
defaultLevel = [hero, monster, item] ++ walls


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    gameLoop . S.fromList =<< mapM identify defaultLevel


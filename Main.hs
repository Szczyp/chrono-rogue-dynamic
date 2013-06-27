module Main where

import Components
import GameLoop
import Types

import Control.Applicative hiding (empty)
import Control.Monad
import System.IO
import Data.Set (empty, fromList, insert)
import Data.UUID (UUID)
import Data.UUID.V4

defaultHero :: Components
defaultHero = Position 2 2                            </>
              Sigil '@'                               </>
              Collision (addInfo "poking...") nothing </>
              Tile nothing nothing                    </>
              Memories empty                          </>
              Sight 2                                 <+>
              Layer 1

monster :: Components
monster = Sigil 'k'                                                           </>
          Collision (addInfo "kobold screams: 'you shall not pass, mortal!'")
                    (addInfo "you poked a giant kobold!")                     <+>
          Position 7 7

item :: Components
item = Sigil '$'                                        </>
       Position 9 8                                     </>
       Tile nothing (addInfo "you see plenty of gold!") <+>
       Memorizable

floors :: Int -> Int -> [Components]
floors horizontal vertical = do
    x <- [1 .. horizontal]
    y <- [1 .. vertical]
    return $ Position x y </>
             Sigil '.'    </>
             Layer 0      <+>
             Memorizable

walls :: Int -> Int -> [Components]
walls horizontal vertical = do
    x <- [1 .. horizontal]
    y <- [1 .. vertical]
    guard $ x `elem` [1, horizontal] || y `elem` [1, vertical]
    return $ Position x y                                                                     </>
             Collision
               nothing
               (addInfo "you touched a cold stone wall, your finger is chilled to the bone!") </>
             Sigil '#'                                                                        </>
             Layer 1                                                                          <+>
             Memorizable

room :: Int -> Int -> [Components]
room horizontal vertical = floors horizontal vertical ++ walls horizontal vertical

defaultLevel :: [Components]
defaultLevel = [monster, item] ++ room 10 10

identify :: Components -> IO Entity
identify cs = Entity <$> nextRandom <*> pure cs

spawnHero :: Level -> IO (UUID, Level)
spawnHero level = do
    hero <- identify defaultHero
    return (uuid hero, insert hero level)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    (heroId, level) <- spawnHero =<< fromList <$> mapM identify defaultLevel
    gameLoop heroId level


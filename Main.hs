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
              Sight 3                                 <+>
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

wall :: Int -> Int -> Components
wall x y = Position x y                                                                    </>
           Collision
             nothing
            (addInfo "you touched a cold stone wall, your finger is chilled to the bone!") </>
           Sigil '#'                                                                       </>
           Opaque                                                                          </>
           Layer 1                                                                         <+>
           Memorizable

walls :: Int -> Int -> [Components]
walls horizontal vertical = do
    x <- [1 .. horizontal]
    y <- [1 .. vertical]
    guard $ x `elem` [1, horizontal] || y `elem` [1, vertical]
    return $ wall x y

room :: Int -> Int -> [Components]
room horizontal vertical = floors horizontal vertical ++ walls horizontal vertical

cShapedWall :: [Components]
cShapedWall = [wall 5 5, wall 6 5, wall 7 5, wall 8 5, wall 5 4, wall 5 3, wall 6 3, wall 7 3, wall 8 3]

defaultLevel :: [Components]
defaultLevel = [monster, item] ++ room 10 10 ++ cShapedWall

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


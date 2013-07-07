{-# LANGUAGE TupleSections #-}

module Systems.Sight where

import Components (Position(..), Sight(..), getPosition, getSightOr, hasOpaque)
import Types
import Utils

import Control.Applicative
import Data.List.Split
import Data.Maybe
import Data.Set (fromList, member, toList)

data Sighted = Sighted { sightedEntity   :: Entity
                       , sightedSight    :: Sight
                       , sightedPosition :: Position }

data Tile = Tile { tileEntity   :: Entity
                 , tilePosition :: Position
                 , tileOpaque   :: Bool }

type Slope = (Int, Int)

type Bounds = (Slope, Slope)

type Column = [Bounds]

inSight :: Sighted -> Level -> Level
inSight (Sighted _ (Sight s) origin @ (Position x y)) level = levelInSight
    where levelInSight = fromList . map tileEntity $ tilesInSight
          tilesInSight = filter ((`elem` concatMap visibleInOctet octets) . tilePosition) tilesInRange
          opaquePositions = fromList
                          . map tilePosition
                          . filter tileOpaque
                          $ tilesInRange
          tilesInRange = filter (inRange . tilePosition)
                       . mapMaybe tile
                       . toList
                       $ level
          tile e = Tile e <$> getPosition e <*> pure (hasOpaque e)
          inRange (Position x' y') = x' >= x - s && x' <= x + s &&
                                     y' >= y - s && y' <= y + s
          visibleInOctet o = (:) origin
                           . concat
                           . take s
                           . zipWith (\ x' -> map (shift origin . o x')) [1 ..]
                           . zipWith concatMap (map inBounds [1 ..])
                           . scan $ (`member` opaquePositions)
                                  . shift origin
                                  . (\ (Position x' y') -> o x' y')
          octets = [ Position
                   , flip Position
                   , \ x' y' -> Position (-y') x'
                   , \ x' y' -> Position (-x') y'
                   , \ x' y' -> Position (-x') (-y')
                   , \ x' y' -> Position (-y') (-x')
                   , \ x' y' -> Position y' (-x')
                   , \ x' y' -> Position x' (-y') ]

inBounds :: Int -> Bounds -> [Int]
inBounds n ((tx, ty), (bx, by)) = [bottom .. top]
    where bottom = if r >= bx then q + 1 else q
              where (q, r) = quotRem (by * (n * 2 - 1)) (bx * 2)
          top = if r > tx then q + 1 else q
              where (q, r) = quotRem (ty * (n * 2 + 1)) (tx * 2)

scan :: (Position -> Bool) -> [Column]
scan isOpaque = scanl column initialColumn [1 ..]
    where initialColumn = [((1, 1), (1, 0))]
          column c n = reverse $ do
            b @ (top, bottom) <- c
            let boundedYs = inBounds n b
            boundedYs' <- split (dropBlanks . dropDelims $ whenElt opaque) boundedYs
            let top' = if maximum boundedYs == maximum boundedYs'
                         then top
                         else topSlope boundedYs'
            let bottom' = if minimum boundedYs == minimum boundedYs'
                            then bottom
                            else bottomSlope boundedYs'
            return (top', bottom')

            where opaque = isOpaque . uncurry Position . (n,)
                  topSlope ys = (x, y)
                      where x = n * 2 + 1
                            y = maximum ys * 2 + 1
                  bottomSlope ys = (x, y)
                      where x = n * 2 - 1
                            y = minimum ys * 2 - 1

sighted :: Entity -> Maybe Sighted
sighted e = Sighted e (getSightOr (Sight 0) e) <$> getPosition e

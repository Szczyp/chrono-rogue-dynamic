module Systems.Draw (draw, drawFor) where

import Components
import Systems
import Types
import Utils

import Prelude hiding (filter, lookup)

import Control.Applicative hiding (empty)
import Control.Arrow
import Data.Function
import Data.List (sortBy)
import Data.Maybe
import Data.Map (Map, filter, fromList, lookup, union)
import Data.Set (empty, toList)
import Data.UUID

data Renderable = Renderable { renderableLayer    :: Layer
                             , renderableSigil    :: Sigil
                             , renderablePosition :: Position }

renderable :: Entity -> Maybe Renderable
renderable e = Renderable (getLayerOr (Layer 1) e)
               <$> getSigil e
               <*> getPosition e

sigil :: Renderable -> Char
sigil Renderable {renderableSigil = (Sigil c)} = c

makePositionMap :: Level -> Map Position Renderable
makePositionMap = fromList
                . map (renderablePosition &&& id)
                . sortBy (compare `on` renderableLayer)
                . mapMaybe renderable
                . toList

drawPositions :: Map Position Renderable -> String
drawPositions positions = ('\n' :) . unlines . map makeRow $ [1 .. 10]
    where makeRow y = map (tile y) [1 .. 10]
          tile y x = maybe ' ' sigil
                   . lookup (Position x y)
                   $ positions

drawFor :: UUID -> Level -> String
drawFor eId level =
    case findEntity eId level of
      Nothing -> "The all consuming void"
      Just e  -> drawPositions $ positionsInSight `union` memorizedPositions
        where positionsInSight = filter inSight' . makePositionMap $ level
              inSight' = maybe (const False) inSight (sighted e) . renderablePosition
              memorizedPositions = makePositionMap memories
              Memories memories = getMemoriesOr (Memories empty) e

draw :: Level -> String
draw = drawPositions . makePositionMap

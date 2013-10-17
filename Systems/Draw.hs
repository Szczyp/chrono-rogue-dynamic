{-# LANGUAGE NoImplicitPrelude #-}

module Systems.Draw (draw, drawFor) where

import Components
import Systems.Sight
import Types
import Utils

import ClassyPrelude

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
makePositionMap = mapFromList
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
        where positionsInSight = makePositionMap $ case sighted e of
                                                     Nothing -> mempty
                                                     Just s -> inSight s level
              memorizedPositions = makePositionMap memories
              Memories memories = getMemoriesOr (Memories mempty) e

draw :: Level -> String
draw = drawPositions . makePositionMap

module Systems.Draw (draw) where

import Components
import Types

import Prelude hiding (lookup)

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List (sortBy)
import Data.Maybe
import Data.Map (fromList, lookup)
import Data.Set (toList)

data Renderable = Renderable { rLayer    :: Layer
                             , rSigil    :: Sigil
                             , rPosition :: Position }

draw :: Level -> String
draw level = ('\n' :) . unlines . map makeRow $ [1 .. 10]
  where makeRow y = map (entityOrFloor y) [1 .. 10]
        entityOrFloor y x = maybe '.' render
                          . lookup (Position x y)
                          $ positions
        render Renderable {rSigil = (Sigil c)} = c
        positions = fromList
                  . map (rPosition &&& id)
                  . sortBy (compare `on` rLayer)
                  . mapMaybe renderable
                  . toList
                  $ level
        renderable e = Renderable (layerOr (Layer 0) e)
                       <$> sigil e
                       <*> position e


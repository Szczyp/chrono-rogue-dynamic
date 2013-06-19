module Systems.Draw (draw) where

import Components
import Types

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data Renderable = Renderable { rLayer    :: Layer
                             , rSigil    :: Sigil
                             , rPosition :: Position }

draw :: Level -> String
draw level = (:) '\n' . unlines . map makeRow $ [1 .. 10]
  where makeRow y = map (entityOrFloor y) [1 .. 10]
        entityOrFloor y x = maybe '.' render . M.lookup (Position x y) . positions $ level
        render Renderable {rSigil = (Sigil c)} = c
        positions = M.fromList . map (rPosition &&& id) . sortBy (compare `on` rLayer) . mapMaybe renderable . S.toList
        renderable e = Renderable (layerOr (Layer 0) e)
                       <$> sigil e
                       <*> position e


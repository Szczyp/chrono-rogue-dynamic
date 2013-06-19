module Systems.Draw (draw) where

import Components
import Types

import Prelude hiding (floor, lookup)

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List hiding (lookup, filter, foldl)
import Data.Map hiding (map, null, filter, mapMaybe, foldl)
import Data.Maybe
import qualified Data.Set as S

data Renderable = Renderable { rLayer    :: Layer
                             , rSigil    :: Sigil
                             , rPosition :: Position }

render :: Renderable -> Char
render Renderable {rSigil = (Sigil c)} = c

draw :: Level -> String
draw level = unlines $ map makeRow [1 .. 10]
  where makeRow y = map (entityOrFloor y) [1 .. 10]
        entityOrFloor y x = render . fromMaybe floor . lookup (Position x y) $ positions
        positions = fromList . map (rPosition &&& id) . sortBy (compare `on` rLayer) . mapMaybe renderable . S.toList $ level
        floor = Renderable (Layer 0) (Sigil '.') (Position 0 0)
        renderable e = Renderable (layerOr (Layer 0) e)
                        <$> sigil e
                        <*> position e


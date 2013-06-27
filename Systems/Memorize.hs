module Systems.Memorize (memorize) where

import Components
import Systems
import Types

import Control.Applicative
import Data.Maybe
import Data.Set (fromList, toList, union)

memorize :: Level -> Level
memorize level = memorizedLevel `union` level
    where memorizedLevel = fromList . map memorize' $ withMemories
          memorize' s @ Sighted {sightedEntity = e} =
            mapC (unionMemories (Memories . levelInSight $ s)) e
          levelInSight s = fromList
                         . map snd
                         . filter (inSight s . fst)
                         $ toMemorize
          toMemorize = mapMaybe memorizable . toList $ level
          withMemories = mapMaybe (getMemories >> sighted) . toList $ level
          memorizable e = do
            _ <- getMemorizable e
            (,) <$> getPosition e <*> pure e
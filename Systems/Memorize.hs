module Systems.Memorize (memorize) where

import Components
import Systems.Sight
import Types

import Data.Maybe
import Data.Set (fromList, toList, union)

memorize :: Level -> Level
memorize level = memorizedLevel `union` level
    where memorizedLevel = fromList . map memorize' $ withMemories
          memorize' s @ Sighted {sightedEntity = e} =
            mapC (unionMemories (Memories . levelInSight $ s)) e
          levelInSight s = inSight s toMemorize
          toMemorize = fromList . mapMaybe memorizable . toList $ level
          withMemories = mapMaybe canMemorize . toList $ level
          memorizable e = getMemorizable e >> return e
          canMemorize e = getMemories e >> sighted e

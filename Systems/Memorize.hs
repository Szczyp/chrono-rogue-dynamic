{-# LANGUAGE NoImplicitPrelude #-}

module Systems.Memorize (memorize) where

import Components
import Systems.Sight
import Types

import ClassyPrelude

memorize :: Level -> Level
memorize level = memorizedLevel `union` level
    where memorizedLevel = setFromList . map memorize' $ withMemories
          memorize' s @ Sighted {sightedEntity = e} =
            mapC (unionMemories (Memories . levelInSight $ s)) e
          levelInSight s = inSight s toMemorize
          toMemorize = setFromList . mapMaybe memorizable . toList $ level
          withMemories = mapMaybe canMemorize . toList $ level
          memorizable e = getMemorizable e >> return e
          canMemorize e = getMemories e >> sighted e

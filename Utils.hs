module Utils where

import Types

import Data.Set
import Data.UUID

iff :: (a -> Bool) -> (a -> a) -> a -> a
iff p f e = if p e then f e else e

convert :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
convert g f = fmap f . g

findEntity :: UUID -> Level -> Maybe Entity
findEntity eId = safeHead . toList . flip intersection (singleton . emptyEntity $ eId)

withId :: UUID -> (Entity -> Level -> Level) -> Level -> Level
withId eId f level = case findEntity eId level of
                             Nothing -> level
                             Just e  -> f e level

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

module Utils where

iff :: (a -> Bool) -> (a -> a) -> a -> a
iff p f e = if p e then f e else e

convert :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
convert g f = fmap f . g

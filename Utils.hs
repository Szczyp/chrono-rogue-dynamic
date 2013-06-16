module Utils where

iff :: (a -> Bool) -> (a -> a) -> a -> a
iff p f a = if p a then f a else a

convert :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
convert g f = fmap f . g

module Utils where

convert :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
convert g f = fmap f . g

module Greenhorn.Stream ( Stream(..) ) where

-- Used to represent an infinite stream of things.
data Stream a = a :< (Stream a)


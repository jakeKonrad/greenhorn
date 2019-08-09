module Greenhorn.Cortex (Cortex, fireCortex) where

import Greenhorn.Genotype
import Data.Functor.Foldable
import Data.Vector.Unboxed
import Prelude hiding (replicate, (++), splitAt, sum, zipWith)

data VecFunc a = VecFunc Int Int (Vector a -> Vector a)

runVecFunc (VecFunc _ _ f) = f

-- The cortex is a directed acyclic multigraph labelled with weights and an activation
-- function.
type Cortex = Damg (Vector Float, Double -> Double)

fireCortex :: Cortex -> Vector Float -> Vector Float
fireCortex = runVecFunc . cata f 
  where
    f EmptyF = VecFunc 0 0 id
    f EdgeF = VecFunc 1 1 id
    f (VertF m n (ws, o)) = VecFunc m n g
      where
        g xs = let ys = zipWith (\x w -> realToFrac x * realToFrac w) xs ws
                   y = realToFrac (o (sum ys))
               in replicate n y 
    f (SwapF m n) = VecFunc s s (\xs -> let (ys, zs) = splitAt m xs in zs ++ ys)
      where
        s = m + n
    f (BesideF (VecFunc m n g) (VecFunc p q h)) = VecFunc (m + p) (n + q) (\xs -> let (ys, zs) = splitAt m xs in g ys ++ h zs)
    f (BeforeF (VecFunc m _ g) (VecFunc _ p h)) = VecFunc m p (g . h)

{-# LANGUAGE TypeFamilies #-}
module Greenhorn.Morphology (Damg, DamgF(..)) where

import Data.Functor.Foldable

-- Data type representing directed acyclic multigraphs as taken 
-- from An Initial Algebra Approach to Directed Acyclic Graphs
-- by Jeremy Gibbons.
data Damg a = Empty
            | Edge
            | Vert Int Int a
            | Swap Int Int
            | Beside (Damg a) (Damg a)
            | Before (Damg a) (Damg a)

instance Functor Damg where
  fmap f (Vert m n a) = Vert m n (f a)
  fmap f (Beside x y) = Beside (fmap f x) (fmap f y)
  fmap f (Before x y) = Before (fmap f x) (fmap f y)
  fmap _ damg         = damg  

-- An unfixed form of Damg
data DamgF a b = EmptyF
               | EdgeF
               | VertF Int Int a
               | SwapF Int Int
               | BesideF b b
               | BeforeF b b

instance Functor (DamgF a) where
  fmap f (BesideF x y) = BesideF (f x) (f y)
  fmap f (BeforeF x y) = BeforeF (f x) (f y)
  fmap _ damgf         = damgf

type instance Base (Damg a) = DamgF a

instance Recursive (Damg a) where
  project Empty        = EmptyF
  project Edge         = EdgeF
  project (Vert m n a) = VertF m n a
  project (Swap m n)   = SwapF m n
  project (Beside x y) = BesideF x y
  project (Before x y) = BeforeF x y 


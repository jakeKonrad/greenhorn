{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Greenhorn.Evolve ( Evolve(..), evolve ) where

import Greenhorn.Stream (Stream)
import Control.Monad (liftM, ap)
import Data.Monoid (Ap(..))

-- This typeclass provides a generic interface, allowing users 
-- to pick-and-choose different genotypes, phenotypes and evolution
-- algorithms. 
class Evolve g p e where
  -- The state of the evolutionary algorithm. This is where the data needed for the
  -- evolutionary algorithm is kept. It is passed to and returned from all methods 
  -- in the form of the state monad (s -> (a, s)) so comments talk about return types without
  -- mentioning the state explicitly.
  data State g p e :: *
  -- The inputs of the phenotypes.
  data Input p :: *
  -- The outputs of the phenotypes.
  data Output p :: *
  -- The fitness of an output.
  data Fitness g p :: *
  -- Returns a population of minimalistic genotypes.
  minimal :: State g p e -> ([g], State g p e)
  -- Maps a genotype to a phenotype.
  toPhenotype :: g -> State g p e -> (p, State g p e)
  -- Given an input and a phenotype returns the output and the tuned
  -- phenotype.
  output :: Input p -> p -> State g p e -> ((Output p, p), State g p e)
  -- Maps a phenotype to a genotype.
  toGenotype :: p -> State g p e -> (g, State g p e)
  -- Given the fitness of a genotype and the genotype, returns the offspring of
  -- the genotype.
  spawn :: Fitness g p e -> g -> State g p e -> ([g], State g p e)
  -- Mutates a genotype.
  mutate :: g -> State g p e -> (g, State g p e)
  -- If the algorithm has reached a stopping condition.
  condition :: State g p e -> Bool

-- Used to thread state through the evolve function.
newtype InternalState s a = InternalState { runInternalState :: s -> (a, s) }

instance Functor (InternalState s) where
  fmap = liftM

instance Applicative (InternalState s) where
  pure x = InternalState (\s -> (x, s))
  (<*>) = ap

instance Monad (InternalState s) where
  stateA >>= k = InternalState (\s -> let (a, s') = runInternalState stateA s in runInternalState (k a) s') 

-- This function evolves a population according to the algorithms given by its type.
-- Its first argument is an optimization function of the algorithm. Its second, 
-- a stream of inputs for the phenotypes to consume. Third, the initial state.
-- It works by first sending the initial state to the minimal method, generating a 
-- minimalistic population and the new state, which it then gives to a loop. On 
-- each iteration of the loop, the stopping condition is checked, and if true the
-- population and end state are returned; otherwise, it maps the genotypes into
-- phenotypes, computes their outputs and the new phenotypes, maps those to fitnesses
-- and genotypes, gathers all the offspring into a new population, and finally mutates
-- all of them.   
evolve :: Evolve g p e
       => (Output g p e -> p -> Fitness g p e)
       -> Stream (Input g p e)
       -> State g p e
       -> ([g], State g p e)
evolve state f = loop (minimal state)
  where
    loop r@(population, state) (x :< xs) 
      | (condition state) = r
      | otherwise         = loop r' xs
      where
        r' = flip runInternalState state $ do
            outputsWithphenotypes <- traverse ((InternalState . (output x)) <=< (InternalState . toPhenotype)) population
            fitnessesWithGenotypes <- traverse (\(y, p) -> (,) <$> pure (f y p) <*> (InternalState (toGenotype p))) outputsWithPhenotypes
            nextGeneration <- getAp $ foldMap (Ap . InternalState . (uncurry spawn)) fitnessesWithGenotypes
            traverse (InternalState . mutate) nextGeneration


{-# LANGUAGE ScopedTypeVariables #-}
module NEAT.Evolve where

import Control.Arrow ((>>>))
import Control.Monad (liftM)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.ST

import Data.STRef
import Data.Random.Normal (normal)

import System.Random

import qualified NEAT.Gene as Gene
 
-- |State of the simulation in state thread s with random generator g
data SimState s g = SimState {
                      simRandGen :: g
                    }

-- |Simulation monad, which is just a Reader with the simulation state,
-- wrapped around an ST monad which keeps track of a sort of heap
type Simulation s g = ReaderT (STRef s (SimState s g)) (ST s)

-- |Get a random value out of our simulation state
getRandom :: forall s g a. (Random a, RandomGen g) => Simulation s g a
getRandom = do
    stref <- ask
    state <- lift . readSTRef $ stref
    let (rnd :: a, gen :: g) = random gen

    lift $ writeSTRef stref $ state { simRandGen = gen }
    return rnd

-- |Get a normal value from the normaldistribution package
getNormal :: forall s g a. (Floating a, Random a, RandomGen g) => Simulation s g a
getNormal = do
    stref <- ask
    state <- lift . readSTRef $ stref
    let (rnd :: a, gen :: g) = normal gen

    lift $ writeSTRef stref $ state { simRandGen = gen }
    return rnd
    
-- |Given a connection gene, randomly mutate its weight
mutateConnGeneWeight :: RandomGen g => Gene.ConnectGene ->
                                      Simulation s g Gene.ConnectGene
mutateConnGeneWeight gene = do
    (rnd :: Double) <- getRandom
    gene' <- if rnd < 0.1
              then do
                (weight :: Double) <- getRandom
                return $ gene { Gene.getConnWeight = weight }
              else do
                (delta :: Double) <- getNormal
                let weight = Gene.getConnWeight gene
                let weight' = weight + delta
                return $ gene { Gene.getConnWeight = weight' }

    return gene'

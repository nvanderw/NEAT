{-# LANGUAGE ScopedTypeVariables #-}
module NEAT.Evolve where

import Control.Arrow ((>>>), Kleisli(..))
import Control.Monad (liftM)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.ST

import Data.STRef
import Data.Random.Normal (normal)

import System.Random

import NEAT.Gene
 
-- |State of the simulation in state thread s with random generator g
data SimState s g = SimState {
                      -- Random number state
                      simRandGen :: g,
                      -- Transfer function of hidden nodes that we add
                      -- during evolution
                      simHidTransfer :: Double -> Double
                    }

-- |Simulation monad, which is just a Reader with the simulation state,
-- wrapped around an ST monad which keeps track of a sort of heap
type Simulation s g = ReaderT (STRef s (SimState s g)) (ST s)

-- |Get a random value out of our simulation state
getRandom :: forall s g a. (Random a, RandomGen g) => Simulation s g a
getRandom = do
    stref <- ask
    state <- lift . readSTRef $ stref
    let (rnd :: a, gen :: g) = random $ simRandGen state

    lift $ writeSTRef stref $ state { simRandGen = gen }
    return rnd

-- |Equivalent of randomR within (Simulation s g) monad
getRandomR :: forall s g a. (Random a, RandomGen g) => (a, a) -> Simulation s g a
getRandomR range = do
    stref <- ask
    state <- lift . readSTRef $ stref
    let (rnd :: a, gen :: g) = randomR range $ simRandGen state

    lift $ writeSTRef stref $ state { simRandGen = gen }
    return rnd

-- |Get a normal value from the normaldistribution package
getNormal :: forall s g a. (Floating a, Random a, RandomGen g) => Simulation s g a
getNormal = do
    stref <- ask
    state <- lift . readSTRef $ stref
    let (rnd :: a, gen :: g) = normal $ simRandGen state

    lift $ writeSTRef stref $ state { simRandGen = gen }
    return rnd
    
-- |Given a connection gene, randomly mutate its weight
mutateConnGeneWeight :: RandomGen g => ConnectGene ->
                                       Simulation s g ConnectGene
mutateConnGeneWeight gene = do
    (rnd :: Double) <- getRandom
    gene' <- if rnd < 0.1
              then do
                (weight :: Double) <- liftM ((-) 1 . (* 2)) getRandom
                return $ gene { cgWeight = weight }
              else do
                (delta :: Double) <- getNormal
                let weight = cgWeight gene
                let weight' = weight + delta
                return $ gene { cgWeight = weight' }

    return gene'

-- Possible mutations 
mutAddNode :: RandomGen g => Genome -> Simulation s g Genome
mutAddNode (Genome nodes conns) = do
    transfer <- liftM simHidTransfer $ ask >>= (lift . readSTRef)
    -- Add a new node to the genome
    let newID = (ngID $ last nodes) + 1
    let newNg = NodeGene {
      ngID = newID,
      ngTransfer = transfer,
      ngType = NodeHid
    }
    
    -- Choose a random connection to split by picking a random index in the
    -- genome.
    rnd_ix <- getRandomR (0, length conns - 1)
    error "unimp: mutAddNode"

mutAddConn :: RandomGen g => Genome -> Simulation s g Genome
mutAddConn = error "unimp: mutAddConn"

mutTweakWeights :: RandomGen g => Genome -> Simulation s g Genome
mutTweakWeights genome = do
    conns' <- mapM mutateConnGeneWeight . gmConns $ genome
    return $ genome { gmConns = conns' }

-- |Helper function to run some kind of mutation (endomorphism) with
-- a given probability in the simulation monad.
perturbWithProb :: RandomGen g => Double -> (a -> Simulation s g a) -> a -> Simulation s g a
perturbWithProb prob f x = do
    (rnd :: Double) <- getRandom
    if rnd < prob
      then f x
      else return x


-- |Run a sequence of mutations of various probabilities on a genome
mutateGenome :: RandomGen g => Genome -> Simulation s g Genome
mutateGenome = runKleisli $
  -- In the Kleisli category (a -> Simulation s g b), chain together
  -- all of the probabilistic mutations
  Kleisli (perturbWithProb 0.1 mutAddNode) >>>
  Kleisli (perturbWithProb 0.1 mutAddConn) >>>
  Kleisli (perturbWithProb 0.9 mutTweakWeights)

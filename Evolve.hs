module NEAT.Evolve where

import Control.Monad.Trans.Reader
import Control.Monad.ST
import Data.STRef

import System.Random

import qualified NEAT.Gene as Gene
 
-- |State of the simulation in state thread s with random generator g
data SimState s g = SimState {
                      simRandGen :: g
                    }

-- |Simulation monad, which is just a Reader with the simulation state,
-- wrapped around an ST monad which keeps track of a sort of heap
type Simulation s g = ReaderT (STRef s (SimState s g)) (ST s)

-- |Given a connection gene, randomly mutate its weight
mutateConnGeneWeight :: RandomGen g => Gene.ConnectGene ->
                                      Simulation s g Gene.ConnectGene
mutateConnGeneWeight gene = error "Unimplemented!"

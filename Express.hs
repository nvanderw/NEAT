module NEAT.Express where

import Control.Monad
import Data.Maybe

import NEAT.Gene
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow ((***))
import Control.Monad.ST
import Data.STRef

data Connection s = Connection (Neuron s) Double
                    deriving (Eq)

instance Ord (Connection s) where
    compare (Connection n1 _) (Connection n2 _) =
        compare (neurID n1) (neurID n2)

-- |Type of a neuron in a state thread s
data Neuron s = Neuron {
    neurID :: Integer,
    -- |Mutable list of connections to forward neurons
    neurConns :: STRef s (Set.Set (Connection s)),

    neurInput :: STRef s Double, -- |Input level
    neurOutput :: STRef s Double -- |Output level
} deriving (Eq)

-- |Mapping from IDs to neurons
type Organism s = Map.Map Integer (Neuron s)

-- |Express a node gene by creating a neuron and adding it to a map from
-- identifiers to neurons
expressNodeGene :: NodeGene -> Organism s -> ST s (Organism s)
expressNodeGene gene organism = do
    connections <- newSTRef Set.empty
    init_input  <- newSTRef 0
    init_output <- newSTRef 0

    let id = ngID gene
    let neuron = Neuron {
      neurID     = id,
      neurConns  = connections,
      neurInput  = init_input,
      neurOutput = init_output
    }
    return $ Map.insert id neuron organism

expressConnGene :: ConnectGene -> Organism s -> ST s ()
expressConnGene (ConnectGene _ _ _ False _) _ = return ()
expressConnGene (ConnectGene inID outID weight True _) organism = do
    let inNeuron = fromJust . Map.lookup inID $ organism
    let outNeuron = fromJust . Map.lookup outID $ organism

    modifySTRef (neurConns inNeuron) $
        Set.insert (Connection outNeuron weight)

expressGenome :: Genome -> Organism s -> ST s (Organism s)
expressGenome (Genome nodeGenes connGenes) org = do
    -- |Express all of our node genes
    org' <- foldM (flip expressNodeGene) org nodeGenes
    -- |Express all connection genes
    mapM_ (`expressConnGene` org') connGenes
    return org'

stepOrganism :: Organism s -> [Double] -> ST s [Double]
stepOrganism org = error "unimp: stepOrganism"

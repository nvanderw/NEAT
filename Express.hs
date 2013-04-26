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
        compare (getNeurID n1) (getNeurID n2)

-- |Type of a neuron in a state thread s
data Neuron s = Neuron {
    getNeurID :: Integer,
    -- |Mutable list of connections to forward neurons
    getNeurConnections :: STRef s (Set.Set (Connection s))
} deriving (Eq)

-- |Mapping from IDs to neurons
type Organism s = Map.Map Integer (Neuron s)

-- |Express a node gene by creating a neuron and adding it to a map from
-- identifiers to neurons
expressNodeGene :: NodeGene -> Organism s -> ST s (Organism s)
expressNodeGene gene organism = do
    connections <- newSTRef Set.empty
    let id = ngID gene
    let neuron = Neuron id connections
    return $ Map.insert id neuron organism

expressConnGene :: ConnectGene -> Organism s -> ST s ()
expressConnGene (ConnectGene _ _ _ False _) _ = return ()
expressConnGene (ConnectGene inID outID weight True _) organism = do
    let inNeuron = fromJust . Map.lookup inID $ organism
    let outNeuron = fromJust . Map.lookup outID $ organism

    modifySTRef (getNeurConnections inNeuron) $
        Set.insert (Connection outNeuron weight)

expressGenome :: Genome -> Organism s -> ST s (Organism s)
expressGenome (Genome nodeGenes connGenes) org = do
    -- |Express all of our node genes
    org' <- foldM (flip expressNodeGene) org nodeGenes
    -- |Express all connection genes
    mapM_ (`expressConnGene` org') connGenes
    return org'

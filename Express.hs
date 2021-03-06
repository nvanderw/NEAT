module NEAT.Express where

import Control.Monad
import Data.Maybe

import NEAT.Gene
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow ((***))
import Control.Monad.ST
import Data.List (sort)
import Data.STRef

data Connection s = Connection (Neuron s) Double
                    deriving (Eq)

instance Ord (Connection s) where
    compare (Connection n1 _) (Connection n2 _) =
        compare (neurID n1) (neurID n2)

-- |Type of a neuron in a state thread s
data Neuron s = Neuron {
    neurID :: Integer,
    neurType :: NodeType,
    neurTransfer :: Double -> Double,

    -- |Mutable list of connections to forward neurons
    neurConns :: STRef s (Set.Set (Connection s)),

    neurInput :: STRef s Double, -- |Input level
    neurOutput :: STRef s Double -- |Output level
}

-- |An equality instance that just compares the neurons' IDs
instance Eq (Neuron s) where
    n1 == n2 = (neurID n1) == (neurID n2)

instance Ord (Neuron s) where
    compare n1 n2 = compare (neurID n1) (neurID n2)

-- |Mapping from IDs to neurons
type Organism s = Map.Map Integer (Neuron s)

emptyOrganism :: Organism s
emptyOrganism = Map.empty

-- |Express a node gene by creating a neuron and adding it to a map from
-- identifiers to neurons
expressNodeGene :: NodeGene -> Organism s -> ST s (Organism s)
expressNodeGene gene organism = do
    connections <- newSTRef Set.empty
    init_input  <- newSTRef 0
    init_output <- newSTRef 0

    let nid = ngID gene

    let neuron = Neuron {
      neurID       = nid,
      neurType     = ngType gene,
      neurTransfer = ngTransfer gene,
      neurConns    = connections,
      neurInput    = init_input,
      neurOutput   = init_output
    }
    return $ Map.insert nid neuron organism

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
stepOrganism org invals = do
    let nodes = sort . map snd . Map.toList $ org
    let inputs = filter ((==) NodeIn . neurType) nodes
    let outputs = filter ((==) NodeOut . neurType) nodes

    -- Save the output values
    outvals <- mapM (readSTRef . neurOutput) outputs

    -- Initialize each node's inputs to zero
    forM_ nodes $ \neuron ->
      writeSTRef (neurInput neuron) 0.0

    -- Add in the inputs from the outside world
    forM_ (zip inputs invals) $ \(neuron, val) ->
      modifySTRef (neurInput neuron) (+ val)

    -- Propagate values forward in the network
    forM_ nodes $ \neuron -> do
      output_level <- readSTRef . neurOutput $ neuron
      conns <- readSTRef . neurConns $ neuron
      -- For each node, iterate over the nodes it feeds to
      forM_ (Set.toList conns) $ \(Connection other weight) ->
        modifySTRef (neurInput other) $ (+) (output_level * weight)

    -- Fire all of the neurons (set their output values to the transfer
    -- function applied to their input values)
    forM_ nodes $ \neuron -> do
      inval <- readSTRef . neurInput $ neuron
      let outval = (neurTransfer neuron) inval
      writeSTRef (neurOutput neuron) $ outval

    return outvals

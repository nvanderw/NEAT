module Tests (testMain) where

import Test.HUnit 
import Control.Monad
import Control.Monad.ST
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)
import Data.STRef (readSTRef)

import qualified Data.Map as Map
import qualified Data.Set as Set

import NEAT.Gene
import NEAT.Express

-- |Tests that a gene is expressed correctly
testSimpleExpress = TestCase $ do
    org <- stToIO $ expressNodeGene (NodeGene 0) Map.empty
    let (Just neuron) = Map.lookup 0 org
    assertEqual "Error in node gene expression" 0 (getNeurID neuron)
    
    org' <- stToIO $ expressNodeGene (NodeGene 1) org
    let org = org'

    -- |Express an inactive connection and check that it doesn't exist
    let connGene = ConnectGene 0 1 1.5 False 0
    stToIO $ expressConnGene connGene org

    conns <- Map.lookup 0 >>> fromJust >>> getNeurConnections >>>
             readSTRef >>> stToIO $ org
    assertEqual "Error in expressing disabled connection" 0 $ Set.size conns

    -- |Now express the same gene when it's enabled
    stToIO $ expressConnGene (connGene { cgEnabled = True }) org
    conns <- Map.lookup 0 >>> fromJust >>> getNeurConnections >>>
            readSTRef >>> stToIO $ org
    assertEqual "Did not express enabled connection" 1 $ Set.size conns

-- |Build up a small genome and try to express it
testExpressGenome = TestCase $ do
    let nodeGenes = map NodeGene [0..4]
    let connGenes = nodeGenes >>=
                      \(NodeGene m) ->
                        nodeGenes >>=
                          \(NodeGene n) ->
                            return (ConnectGene m n 1.0 True (m * 5 + n))

    -- Express the organism
    org <- stToIO $ expressGenome (Genome nodeGenes connGenes) Map.empty

    -- Test that all neurons have 5 connections
    forM_ (Map.assocs org) $ \(id, neuron) -> do
        conns <- stToIO $ readSTRef $ getNeurConnections neuron
        let message = "Neuron " ++ show id ++ " has wrong number of connections"
        assertEqual message 5 $ Set.size conns

testMain = runTestTT $ TestList [testSimpleExpress, testExpressGenome]

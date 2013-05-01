module NEAT.Tests (testMain) where

import Test.HUnit 
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Reader
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)
import Data.STRef (readSTRef, newSTRef)
import Data.List (genericReplicate)
import System.Random (getStdGen)

import qualified Data.Map as Map
import qualified Data.Set as Set

import NEAT.Gene
import NEAT.Express
import NEAT.Evolve

-- |Tests that a gene is expressed correctly
testSimpleExpress = TestCase $ do
    let ng0 = NodeGene {
      ngID       = 0,
      ngType     = NodeIn,
      ngTransfer = id
    } 

    org <- stToIO $ expressNodeGene ng0 Map.empty
    let (Just neuron) = Map.lookup 0 org
    assertEqual "Error in node gene expression" 0 (neurID neuron)
    
    let ng1 = NodeGene {
      ngID       = 1,
      ngType     = NodeOut,
      ngTransfer = id
    }

    org' <- stToIO $ expressNodeGene ng1 org
    let org = org'

    -- |Express an inactive connection and check that it doesn't exist
    let connGene = ConnectGene {
      cgInID    = 0,
      cgOutID   = 1,
      cgWeight  = 1.5,
      cgEnabled = False,
      cgInnov   = 0
    }

    stToIO $ expressConnGene connGene org

    conns <- Map.lookup 0 >>> fromJust >>> neurConns >>>
             readSTRef >>> stToIO $ org
    assertEqual "Error in expressing disabled connection" 0 $ Set.size conns

    -- |Now express the same gene when it's enabled
    stToIO $ expressConnGene (connGene { cgEnabled = True }) org
    conns <- Map.lookup 0 >>> fromJust >>> neurConns >>>
            readSTRef >>> stToIO $ org
    assertEqual "Did not express enabled connection" 1 $ Set.size conns

-- |Build up a small genome and try to express it
testExpressGenome = TestCase $ do
    let nodeIDs = [0..4]
    let nodeGenes = flip map nodeIDs $ \nid -> NodeGene {
      ngID       = nid,
      ngType     = NodeIn,
      ngTransfer = id
    }

    -- Connect all nodeIDs to all others
    let connGenes = nodeIDs >>=
                      \m ->
                        nodeIDs >>=
                          \n ->
                            return $ ConnectGene {
                              cgInID = m,
                              cgOutID = n,
                              cgWeight = 1.0,
                              cgEnabled = True,
                              cgInnov = m * 5 + n
                            }

    -- Express the organism
    org <- stToIO $ expressGenome (Genome nodeGenes connGenes) Map.empty

    -- Test that all neurons have 5 connections
    forM_ (Map.assocs org) $ \(id, neuron) -> do
        conns <- stToIO $ readSTRef $ neurConns neuron
        let message = "Neuron " ++ show id ++ " has wrong number of connections"
        assertEqual message 5 $ Set.size conns

testMutateGene = TestCase $ do
    let connGene = ConnectGene {
      cgInID  = 0,
      cgOutID = 1,
      cgWeight = 0.0,
      cgEnabled = True,
      cgInnov = 0
    }

    -- Set up a simulation state. We have to do some types wrangling for
    -- this.
    gen <- getStdGen
    let state = SimState { simRandGen = gen }

    let gene' = runST $ do
        -- Create a mutable reference for the state
        stref <- newSTRef state
        runReaderT (mutateConnGeneWeight connGene) stref
    
    let weight = cgWeight gene'

    -- This is probabilistic but should hold for at least 99.9937% of cases
    assertBool "Mutated connection weight outside expected range" $
      (-4 < weight) && (weight < 4)

testStepSimpleOrganism = TestCase $ do
    let ngIn = NodeGene {
      ngID       = 0,
      ngTransfer = id,
      ngType     = NodeIn
    }

    let ngOut = NodeGene {
      ngID       = 1,
      ngTransfer = id,
      ngType     = NodeOut
    }

    let cg = ConnectGene {
      cgInID    = 0,
      cgOutID   = 1,
      cgWeight  = 1.0,
      cgEnabled = True,
      cgInnov   = 0
    }

    let genome = Genome [ngIn, ngOut] [cg]

    let outputs = runST $ do
        organism <- expressGenome genome emptyOrganism
        sequence . replicate 3 $ stepOrganism organism [1.0]

    assertEqual "Values not propagating in small network as expected" outputs
      [[0], [0], [1]]

testStepBigOrganism = TestCase $ do
    let numnodes = 500

    let ngIn = NodeGene {
      ngID       = 0,
      ngTransfer = id,
      ngType     = NodeIn
    }

    let ngOut = NodeGene {
      ngID       = numnodes - 1,
      ngTransfer = id,
      ngType     = NodeOut
    }

    let ngHiddens = flip map [1..numnodes - 2] $ \n -> NodeGene {
          ngID = n,
          ngTransfer = id,
          ngType = NodeHid
    }

    let ngs = ngIn : ngHiddens ++ [ngOut]
    let cgs = flip map [0..numnodes - 2] $ \n -> ConnectGene {
          cgInID    = n,
          cgOutID   = n + 1,
          cgWeight  = 1.0,
          cgEnabled = True,
          cgInnov   = n
    }

    let genome = Genome ngs cgs
    
    let outputs = runST $ do
        organism <- expressGenome genome emptyOrganism
        sequence . genericReplicate (numnodes + 1) $ stepOrganism organism [1.0]

    assertEqual "Values not propagating in large network as expected" outputs
      $ genericReplicate numnodes [0.0] ++ [[1.0]]

testMain = runTestTT $ TestList [testSimpleExpress,
                                 testExpressGenome,
                                 testMutateGene,
                                 testStepSimpleOrganism,
                                 testStepBigOrganism]

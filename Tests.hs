module Tests (testMain) where

import Test.HUnit 
import Control.Monad.ST
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)
import Data.STRef (readSTRef)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified NEAT.Gene as Gene
import qualified NEAT.Express as Xpr

-- |Tests that a gene is expressed correctly
testExpression = TestCase $ do
    org <- stToIO $ Xpr.expressNodeGene (Gene.NodeGene 0) Map.empty
    let (Just neuron) = Map.lookup 0 org
    assertEqual "Error in node gene expression" 0 (Xpr.getNeurID neuron)
    
    org' <- stToIO $ Xpr.expressNodeGene (Gene.NodeGene 1) org
    let org = org'

    -- |Express an inactive connection and check that it doesn't exist
    let connGene = Gene.ConnectGene 0 1 1.5 False 0
    stToIO $ Xpr.expressConnGene connGene org

    conns <- Map.lookup 0 >>> fromJust >>> Xpr.getNeurConnections >>>
             readSTRef >>> stToIO $ org
    assertEqual "Error in expressing disabled connection" 0 $ Set.size conns

    -- |Now express the same gene when it's enabled
    stToIO $ Xpr.expressConnGene (connGene { Gene.getConnEnabled = True }) org
    conns <- Map.lookup 0 >>> fromJust >>> Xpr.getNeurConnections >>>
            readSTRef >>> stToIO $ org
    assertEqual "Did not express enabled connection" 1 $ Set.size conns
    

testMain = runTestTT testExpression

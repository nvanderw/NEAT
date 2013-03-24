module NEAT.Gene (NodeGene(..), 
                  ConnectGene(..),
                  Genome(..)) where

data NodeGene = NodeGene {
    getNodeID :: Integer
}

data ConnectGene = ConnectGene {
    -- |ID of first connected node
    getConnInID :: Integer,
    -- |ID of second connected node
    getConnOutID :: Integer,
    -- |Weight of connection between nodes
    getConnWeight :: Double,
    -- |Is this connection enabled?
    getConnEnabled :: Bool,
    -- |Globally-incremented "innovation number"
    getInnov :: Integer
}
 
data Genome = Genome {
    getGenomeNodes :: [NodeGene],
    getGenomeConns :: [ConnectGene]
}

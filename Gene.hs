module NEAT.Gene (NodeGene(..), 
                  ConnectGene(..),
                  Genome(..)) where

data NodeGene = NodeGene {
    ngID :: Integer
}

data ConnectGene = ConnectGene {
    -- |ID of first connected node
    cgInID :: Integer,
    -- |ID of second connected node
    cgOutID :: Integer,
    -- |Weight of connection between nodes
    cgWeight :: Double,
    -- |Is this connection enabled?
    cgEnabled :: Bool,
    -- |Globally-incremented "innovation number"
    cgInnov :: Integer
}
 
data Genome = Genome {
    gmNodes :: [NodeGene],
    gmConns :: [ConnectGene]
}

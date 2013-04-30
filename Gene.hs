module NEAT.Gene (NodeGene(..), 
                  NodeType(..),
                  ConnectGene(..),
                  Genome(..)) where

-- |A node can be input, output, or a hidden node that applies some
-- transformation to the sum of its inputs
data NodeType = NodeIn | NodeOut | NodeHid (Double -> Double)

data NodeGene = NodeGene {
    ngID :: Integer,
    ngType :: NodeType
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

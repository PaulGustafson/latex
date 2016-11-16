-- Aids in computing the evaluation of a stringnet

-- Edges are labeled by objects of the input category
type Edge = String

-- Deals with parenthesization
data EdgeTree = Leaf Edge
                | Node EdgeTree EdgeTree

-- Vertices are represented by lists of outgoing edges in CCW order
type Vertex = (EdgeTree, Morphism)

-- Morphism in the input category
type Morphism = String 


-- Preconditions:
-- First vertex is in state X_1 X_2 ... X_N
-- Second vertex is in state Y_1 Y_2 ... Y_M X_N*
-- Both parenthesizations are the defaults
compose :: Vertex -> Vertex -> Vertex
compose (Leaf l) EdgeTree

-- Represent the graph using an adjacency matrix with entries in Obj(A)
-- This matrix should be equal to its conjugate transpose.
-- We also need to keep track of the order of edges out of each vertex
-- along with the parenthesization.  This should be represented by a
-- binary tree whose leaves are the outgoing edges.


-- index of braid move diagram
type Diagram = Int

type Edge = Int

type Vertex = Int

type Object = String

data Morphism = Morphism { source :: Object
                         , target :: Object }


source :: Diagram -> Edge -> Vertex


vLabel :: Vertex -> Morphism
vLabel 1 = "(k^{-1} \otimes l \otimes k) \otimes (g^{-1} \otimes h \otimes g)"

eLabel :: Edge -> Object
eLabel 

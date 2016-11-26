-- Aids in computing the evaluation of a stringnet

-- Edges are labeled by objects of the input category
type Object = String

-- Binary tree of objects
-- Deals with parenthesization
data Tree = Leaf Object
          | Node Tree Tree

-- Vertices correspond to lists of outgoing edges in CCW order
-- and the morphism label.  
type Vertex = (Tree, Morphism)

-- Morphism in the input category
type Morphism = String 

-- Remove rightmost leaf
--
removeRight :: Tree -> Tree
removeRight Node leftTree (Leaf l) = Node leftTree 
removeRight Leaf _ = error "Can't prune size 1 tree"



-- Preconditions:
-- First vertex is in state X_1 X_2 ... X_N
-- Second vertex is in state Y_1 Y_2 ... Y_M X_N*
-- with some choices of parenthesization
compose :: Vertex -> Vertex -> Vertex
compose (Leaf l) EdgeTree

-- Aids in computing the evaluation of a stringnet


module Test where
  
  postulate
    String : Set
  
  {-# BUILTIN STRING  String #-}

  -- Objects of the input category are currently represented by strings
  Object : Set
  Object = String

  -- Binary tree
  -- Deals with parenthesization
  data Tree : (T : Set) → Set where
       Leaf : (T : Set) → Tree T
       Node : Tree → Tree → Tree


-- -- Vertices are represented by lists of outgoing edges in CCW order
-- data Vertex = (EdgeTree, Morphism)

-- -- Morphism in the input category
-- data Morphism = String 

-- -- Remove rightmost leaf
-- removeRight :: Tree -> Tree
-- removeRight Leaf


-- -- Preconditions:
-- -- First vertex is in state X_1 X_2 ... X_N
-- -- Second vertex is in state Y_1 Y_2 ... Y_M X_N*
-- -- with some choices of parenthesization
-- compose :: Vertex -> Vertex -> Vertex
-- compose (Leaf l) EdgeTree

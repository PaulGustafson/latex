import           Data.List

-- Encode as marked CW-complex.
-- TODO: Finish computing TwoComplex transformations for figures
-- TODO: Make Edge equality take disks into account.  Currently, two
-- Connect Edges are equal if they have the same endpoints.  This is a bug.
--
-- IDEA: Refactor using Simplex n

-- Left and right refer to positions before the braiding operation
-- TODO: Separate into interior vertex and puncture types
data Vertex = Main | LeftPuncture | RightPuncture | Midpoint Edge | Contract Edge
  deriving (Show, Eq)

-- Orientations of initial edges are given by arrows in the figures in the paper
data Edge = LeftLoop | RightLoop | LeftLeg | RightLeg -- initial edges
          | FirstHalf Edge | SecondHalf Edge -- result of adding coev vertex
          | Connect Vertex Vertex -- connecting with a 1 edge
          | TensorE Edge Edge     -- stick together parallel edges
          | ContractedNeighbor
            { contractedEdge :: Edge
            , preimage       :: Edge
            }
          deriving (Show, Eq)

data TwoComplex = TwoComplex
                  { vertices :: [Vertex]
                  , edges    :: [Edge]
                  } deriving (Show)

-- data Disk = OutsideDisk | LeftDisk | RightDisk | Cut Disk Vertex Vertex
--           deriving (Show)

data Object = G | H | K | L | One | Star Object | TensorO Object Object

-- TODO: add typing to Compose
data Morphism = Phi | Id Object | Coev Object | Ev Object | TensorM Morphism Morphism | Compose Morphism Morphism

data Orientation = Plus | Minus

endpoints :: Edge -> [Vertex]
endpoints LeftLoop  = [Main, Main]
endpoints RightLoop = [Main, Main]
endpoints LeftLeg   = [Main, LeftPuncture]
endpoints RightLeg  = [Main, RightPuncture]
endpoints (FirstHalf e) = [(endpoints e) !! 0, Midpoint e]
endpoints (SecondHalf e) = [Midpoint e, (endpoints e) !! 1]
endpoints (Connect v1 v2) = [v1, v2]
endpoints (TensorE e1 _) = endpoints e1
endpoints (ContractedNeighbor c p)  =
  [if ep `elem` endpoints c then Contract c else ep | ep <- endpoints p]


start :: Edge -> Vertex
start e = (endpoints e) !! 0

end :: Edge -> Vertex
end e = (endpoints e) !! 1

-- TODO: deal with edge compositions changing this function
-- perimeter :: Disk -> [(Edge, Orientation)]
-- perimeter OutsideDisk = [(LeftLoop, Plus), (RightLoop, Plus)]
-- perimeter LeftDisk    = [(LeftLoop, Minus), (LeftLeg, Plus), (LeftLeg, Minus)]
-- perimeter RightDisk   = [(RightLoop, Minus), (RightLeg, Plus), (RightLeg, Minus)]
-- perimeter Cut d v1 v2 = [(Connect v2 v1, Plus)] ++ (takeWhile (f v2) dropWhile (f v1) $ cycle $ perimeter d)
--                               where f v (e, o) = v /= start e

objectLabel :: Edge -> Object
objectLabel LeftLoop = G
objectLabel LeftLeg = H
objectLabel RightLoop = K
objectLabel RightLeg = L
objectLabel (FirstHalf e) = objectLabel e
objectLabel (SecondHalf e) = Star (objectLabel e)
objectLabel (Connect v1 v2) = One

-- inexhaustive-matching  (Punctures don't have labels, c.f. comments at Vertex definition)
morphismLabel :: Vertex -> Morphism
-- morphismLabel LeftPuncture = Nothing
-- morphismLabel RightPuncture = Nothing
morphismLabel Main = Phi
morphismLabel (Midpoint e) = Coev $ objectLabel e
morphismLabel (Contract e) = Compose (Ev $ objectLabel e) (TensorM (morphismLabel (start e)) (morphismLabel (end e)))

touches :: Edge -> Edge -> Bool
touches e1 e2 = if  length (intersect (endpoints e1) (endpoints e2)) > 0
                                        then True
                                        else False

contract :: Edge -> TwoComplex -> TwoComplex
contract contractedEdge tc = TwoComplex
                { vertices = [Contract contractedEdge] ++ [v | v <- vertices tc, not $ v `elem` (endpoints contractedEdge)]
                , edges = [if contractedEdge `touches` e
                           then ContractedNeighbor contractedEdge e
                           else e
                          | e <- edges tc, contractedEdge /= e]
                }

connect :: Vertex -> Vertex -> TwoComplex -> TwoComplex
connect v1 v2 tc = TwoComplex
                   {  vertices = vertices tc
                   ,  edges = [Connect v1 v2] ++ edges tc
                   }

addCoev :: Edge -> TwoComplex -> TwoComplex
addCoev e tc = TwoComplex
               { vertices = [Midpoint e] ++ vertices tc
               , edges = [FirstHalf e, SecondHalf e] ++ [f | f <- edges tc, f /= e]
               }

-- tcX corresponds to figure number X from the paper
tc1 = TwoComplex { vertices = [Main, LeftPuncture, RightPuncture]
                 , edges    = [LeftLoop, RightLoop, LeftLeg, RightLeg]
                 }
tc2 = tc1
tc2_5 = addCoev RightLoop $ addCoev (SecondHalf LeftLoop) $ addCoev LeftLeg $ addCoev LeftLoop tc2
newVertices = take 4 $ vertices tc2_5
tc3 = foldl f tc2_5  [(x,y) | x <- newVertices, y <- cycle $ tail newVertices]
      where f a (x,y)  = connect x y a



v1 = vertices tc1
v2 = v1
-- topX means X from the top
top1 = Midpoint LeftLoop
top2 = Midpoint LeftLeg
top3 = Midpoint (SecondHalf LeftLoop)
top4 = Midpoint RightLoop
v3 = v1 ++ [top1, top2, top3, top4]
e12 = Connect top1 top2
e23 = Connect top2 top3
e34 = Connect top3 top4

-- top12V = (Contract top1)









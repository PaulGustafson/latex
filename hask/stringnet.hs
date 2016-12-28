import           Control.Monad.State
import           Data.List

-- Encode as marked CW-complex.
-- TODO: Finish computing TwoComplex transformations for figures
-- TODO: Make Edge equality take disks into account.  Currently, two
-- Connect Edges are equal if they have the same endpoints.  This is a bug.
--
-- TODO: Typify stack-like usage of vertices, edges
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
          | Reverse Edge
          deriving (Show, Eq)

-- instance Show Edge where
--   show LeftLoop = "LeftLoop"
--   show RightLoop = "RightLoop"
--   show LeftLeg = "LeftLeg"
--   show RightLeg = "RightLeg"
--   show (FirstHalf e) = show e ++ "0"
--   show (SecondHalf e) = show e ++ "1"
--   show (Connect v1 v2) = (show v1) ++ "-" ++ (show v2)
--   show (TensorE e1 e2) = "(" ++  (show e1) ++ ")*(" ++ (show e2) ++ ")"

data TwoComplex = TwoComplex
                  { vertices :: [Vertex]
                  , edges    :: [Edge]
                  , image :: Vertex -> Vertex
                  , morphismLabel :: Vertex -> Morphism
                  } 

-- data Disk = OutsideDisk | LeftDisk | RightDisk | Cut Disk Vertex Vertex
--           deriving (Show)
data Object = G | H | K | L | One | Star Object | TensorO Object Object
            deriving (Show)
                                                           
-- TODO: add typing to Compose
data Morphism = Phi | Id Object | Coev Object | Ev Object | TensorM Morphism Morphism | Compose Morphism Morphism
              deriving (Show)

star :: Object -> Object
star (Star o) = o
star o = Star o

initialEndpoints :: Edge -> [Vertex]
initialEndpoints edge  = case edge of
  LeftLoop  -> [Main, Main]
  RightLoop -> [Main, Main]
  LeftLeg   -> [Main, LeftPuncture]
  RightLeg  -> [Main, RightPuncture]
  FirstHalf e -> [(initialEndpoints e) !! 0, Midpoint e]
  SecondHalf e -> [Midpoint e, (initialEndpoints e) !! 1]
  Connect v1 v2 -> [v1, v2]
  TensorE e1 _ -> initialEndpoints e1
  Reverse e -> reverse $ initialEndpoints e

initialStart :: Edge -> Vertex
initialStart e =  (initialEndpoints e) !! 0

initialEnd :: Edge -> Vertex
initialEnd e =  (initialEndpoints e) !! 1

endpoints :: Edge -> TwoComplex -> [Vertex]
endpoints e tc = map (image tc) $ initialEndpoints e

start :: Edge -> TwoComplex -> Vertex
start e tc =  (endpoints e tc) !! 0

end :: Edge -> TwoComplex -> Vertex
end e tc =  (endpoints e tc) !! 1



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
objectLabel (SecondHalf e) = objectLabel e
objectLabel (Connect v1 v2) = One
objectLabel (TensorE e1 e2) = TensorO (objectLabel e1) (objectLabel e2)
objectLabel (Reverse e)  = star (objectLabel e)

                                       

reverseEdge :: Edge -> State TwoComplex Edge
reverseEdge e0 = state $ \tc ->
  let rev (Reverse e1) = e1
      rev e1 = Reverse e1 in
  (rev e0
  , tc
       { edges = [rev e0] ++ [e | e <- edges tc
                                    , e /= e0] })

tensor :: Edge -> Edge -> State TwoComplex Edge
tensor e1 e2 = state $ \tc -> let product = TensorE e1 e2 in
  (product, tc { edges = [product] ++ [e | e <- edges tc, e /= e1, e /= e2] })


contract :: Edge -> State TwoComplex Vertex
contract contractedEdge  = state $ \tc ->
  let composition = Contract contractedEdge in
  (composition, TwoComplex
                { vertices = [composition] ++
                             [v | v <- vertices tc
                                , not $ v `elem` (endpoints contractedEdge tc)]
                , edges = [e | e <- edges tc
                             , contractedEdge /= e]
                , image = (\v -> if (v `elem` (endpoints contractedEdge tc))
                                 then composition
                                 else v
                          ) . (image tc)
                , morphismLabel = (\v -> if (v == composition) 
                                         then  Compose (Ev $ objectLabel contractedEdge)
                                               (TensorM (morphismLabel tc (start contractedEdge tc))
                                                (morphismLabel tc (end contractedEdge tc)))
                                         else morphismLabel tc v )
                }
  )

connect :: Vertex -> Vertex -> State TwoComplex Edge
connect v1 v2  = state $ \tc -> 
  let connection = Connect v1 v2 in
  ( connection, tc { edges = [connection] ++ edges tc} )

addCoev :: Edge -> State TwoComplex (Vertex, Edge, Edge)
addCoev e = state $ \tc ->
  let mp  = Midpoint e
      fh = FirstHalf e
      sh = SecondHalf e in
  ((mp, fh, sh), tc 
                { vertices =  [mp] ++ vertices tc
                , edges = [fh, sh] ++ [f | f <- edges tc
                                         , f /= e]
                , morphismLabel = \v -> if v == mp
                                        then Coev $ objectLabel e
                                        else morphismLabel tc v
                }
  )

-- tcX corresponds to figure number X from the paper
initialTC = TwoComplex { vertices = [Main, LeftPuncture, RightPuncture]
                       , edges    = [LeftLoop, RightLoop, LeftLeg, RightLeg]
                       , image    = id
                       , morphismLabel  =  (\m -> case m of Main -> Phi)
                       }
            
slide = do
  (top1,lt1,_) <- addCoev LeftLoop
  (top2,lt2,rt2) <- addCoev LeftLeg
  (top3,rt13,lt3) <- addCoev (SecondHalf LeftLoop)
  (top4,lt4,rt4) <- addCoev RightLoop
  e1 <- connect top1 top2
  e2 <- connect top2 top3
  e3 <- connect top3 top4
  contract e1
  contract e2
  contract e3
  rlt3 <- reverseEdge lt3
  lt43 <- tensor lt4 rlt3
  lt42 <- tensor lt43 lt2
  lt41 <- tensor lt42 lt1
  contract rt4


finalTC = execState slide initialTC


-- tc4 = foldl f tc3 [1..3]
--   where f a _ = contract (edges a !! 0) a
-- e4 = edges tc4
-- tc5 = tensor (e4) $ tensor (e4 !! 0) (e4 !! 3) tc4

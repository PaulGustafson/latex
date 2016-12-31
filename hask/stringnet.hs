import           Control.Monad.State
import           Data.List

-- Encode a stringnet as a marked CW-complex.
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
          | FirstHalf Edge | SecondHalf Edge -- result of adding coev vertex (--(e)-->(coev e) --(e) -->
          | Connect Edge Edge Disk -- connects the start of the two edges with a 1 in the disk
          | TensorE Edge Edge     -- stick together parallel edges
          | Reverse Edge -- don't use this constructor except to pattern match, use "rev" instead
          deriving (Show, Eq)


data Disk = Outside | LeftDisk | RightDisk
          | Cut Edge  -- Edge should be of type Connect
          deriving (Show, Eq)

data Tree a = Node (Tree a) (Tree a) | Leaf a
            deriving (Eq)

data TwoComplex = TwoComplex
                  { vertices      :: [Vertex]
                  , edges         :: [Edge]
                  , disks         :: [Disk]                  
                  , image         :: Vertex -> Vertex
                  , morphismLabel :: Vertex -> Morphism
                  , edgeTree      :: Vertex -> Tree Edge  --outgoing orientation
                  } 

data Object = G | H | K | L
            | One
            | Star Object  -- Don't use this constructor except to pattern match, use "star" instead
            | TensorO Object Object
            deriving (Show)
                                                           
data Morphism = Phi | Id Object | Coev Object | Ev Object | TensorM Morphism Morphism | Compose Morphism Morphism
              deriving (Show)


rev :: Edge -> Edge
rev (Reverse e) = e
rev e = Reverse e

star :: Object -> Object
star (Star o) = o
star o = Star o

-- endpoints before finding the images of the vertices under contractions
initialEndpoints :: Edge -> [Vertex]
initialEndpoints edge  = case edge of
  LeftLoop  -> [Main, Main]
  RightLoop -> [Main, Main]
  LeftLeg   -> [Main, LeftPuncture]
  RightLeg  -> [Main, RightPuncture]
  FirstHalf e -> [(initialEndpoints e) !! 0, Midpoint e]
  SecondHalf e -> [Midpoint e, (initialEndpoints e) !! 1]
  Connect e1 e2 _ -> [initialStart e1, initialStart e2]
  TensorE e1 _ -> initialEndpoints e1
  Reverse e -> reverse (initialEndpoints e)

initialStart :: Edge -> Vertex
initialStart e =  (initialEndpoints e) !! 0

initialEnd :: Edge -> Vertex
initialEnd e =  (initialEndpoints e) !! 1

endpoints :: Edge -> TwoComplex -> [Vertex]
endpoints e tc = map (image tc) (initialEndpoints e)

start :: Edge -> TwoComplex -> Vertex
start e tc =  (endpoints e tc) !! 0

end :: Edge -> TwoComplex -> Vertex
end e tc =  (endpoints e tc) !! 1


perimeter :: Disk -> TwoComplex -> [Edge]
perimeter Outside   _  = [LeftLoop, RightLoop]
perimeter LeftDisk  _  = [Reverse LeftLoop, LeftLeg, Reverse LeftLeg]
perimeter RightDisk _  = [Reverse RightLoop, RightLeg, Reverse RightLeg]
perimeter (Cut c@(Connect e1 e2 d)) tc = [c] ++ (takeWhile (/= e1) $ dropWhile (/= e2) $ cycle $ perimeter d tc)
perimeter (Cut c@(Reverse (Connect e1 e2 d))) tc = [c] ++ (takeWhile (/= e2) $ dropWhile  (/= e1) $ cycle $ perimeter d tc)


objectLabel :: Edge -> Object
objectLabel LeftLoop = G
objectLabel LeftLeg = H
objectLabel RightLoop = K
objectLabel RightLeg = L
objectLabel (FirstHalf e) = objectLabel e
objectLabel (SecondHalf e) = objectLabel e
objectLabel (Connect _ _ _) = One
objectLabel (TensorE e1 e2) = TensorO (objectLabel e1) (objectLabel e2)
objectLabel (Reverse e)  = star (objectLabel e)


reverseEdge :: Edge -> State TwoComplex Edge
reverseEdge e0 = state $ \tc ->
  (rev e0
  , tc
       { edges = [rev e0] ++ [e | e <- edges tc
                                    , e /= e0] })

flatten :: Tree a -> [a]
flatten (Leaf x) = [x] 
flatten (Node x y) = (flatten x) ++ (flatten y)

associateR :: Tree a -> Tree a
associateR (Node (Node x y) z) = Node x (Node y z)

associateL :: Tree a -> Tree a
associateL (Node x (Node y z)) = Node (Node x y) z

replace :: (Eq a) => Tree a -> Tree a -> Tree a -> Tree a
replace subTree1 subTree2 bigTree = 
  if bigTree == subTree1
  then subTree2
  else case bigTree of
    Leaf x  -> Leaf x
    Node x y -> Node (replace subTree1 subTree2 x)
                (replace subTree1 subTree2 y)

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
                , edgeTree = \v -> if v == composition
                                   then Node (edgeTree tc $ start contractedEdge tc) (edgeTree tc $ end contractedEdge tc)
                                   else edgeTree tc v
                }
  )


-- Connect the starting point of the first edge to that of the second
-- through the disk
connect :: Edge -> Edge -> Disk -> State TwoComplex Edge
connect e1 e2 d = state $ \tc -> 
  let connection = Connect e1 e2 d in
  ( connection, tc { edges = [connection] ++ edges tc
                   , disks = [Cut connection, Cut $ rev connection]
                             ++ [d2 | d2 <- disks tc
                                    , d2 /= d]
                   , edgeTree = \v -> f v                   
                   } )
    where
      f v
        | v == start e1 = replace (Leaf e1) (Node (Leaf e1) (Leaf $ rev connection)) 
        | v == start e2 = replace (Leaf e2) (Node (Leaf e1) (Leaf $ connection)) 
        | otherwise     = edgeTree tc
      
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
                , edgeTree = \v -> if v == mp
                                   then Node (Leaf $ rev $ FirstHalf e) (Leaf $ SecondHalf e)
                                   else edgeTree tc v
                }
  )

-- tcX corresponds to figure number X from the paper
initialTC = TwoComplex { vertices = [Main, LeftPuncture, RightPuncture]
                       , edges    = [LeftLoop, RightLoop, LeftLeg, RightLeg]
                       , disks    = [Outside, LeftDisk, RightDisk]
                       , image    = id
                       , morphismLabel  =  (\m -> case m of Main -> Phi)
                       , edgeTree = \v -> case v of Main ->
                                                       Node
                                                         (Node
                                                          (Node
                                                           (Leaf LeftLoop)
                                                           (Leaf LeftLeg)
                                                          )
                                                          (Leaf $ Reverse LeftLoop)
                                                         )
                                                         (Node
                                                          (Node
                                                           (Leaf RightLoop)
                                                           (Leaf RightLeg)
                                                          )
                                                          (Leaf $ Reverse RightLoop)
                                                         )
                       }
            
-- slide = do
--   (top1,lt1,_) <- addCoev LeftLoop
--   (top2,lt2,rt2) <- addCoev LeftLeg
--   (top3,rt13,lt3) <- addCoev (SecondHalf LeftLoop)
--   (top4,lt4,rt4) <- addCoev RightLoop
--   e1 <- connect top1 top2
--   e2 <- connect top2 top3
--   e3 <- connect top3 top4
--   contract e1
--   contract e2
--   contract e3
--   rlt3 <- reverseEdge lt3
--   lt43 <- tensor lt4 rlt3
--   lt42 <- tensor lt43 lt2
--   lt41 <- tensor lt42 lt1
--   contract rt4

-- finalTC = execState slide initialTC






-- instance Show Edge where
--   show LeftLoop = "LeftLoop"
--   show RightLoop = "RightLoop"
--   show LeftLeg = "LeftLeg"
--   show RightLeg = "RightLeg"
--   show (FirstHalf e) = show e ++ "0"
--   show (SecondHalf e) = show e ++ "1"
--   show (Connect v1 v2) = (show v1) ++ "-" ++ (show v2)
--   show (TensorE e1 e2) = "(" ++  (show e1) ++ ")*(" ++ (show e2) ++ ")"

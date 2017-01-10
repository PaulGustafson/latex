
-- Encode a stringnet as a marked CW-complex.
-- For now, we assume left and right duals are the same
--
-- TODO: Find where the infinite loop is showing up in isolate2
--             by tracing code?  DEMONADIFY
--
-- TODO: Show and read for Tree
-- 
-- TODO: Do everything in terms of left duals
--
-- TODO: Refactor interior TC methods into State methods
--
-- TODO: Refactor using Simplex n

-- Notes: Should I be function-centric instead of TwoComplex-centric?
-- For example, can I pull morphismLabel out?
--

module Stringnet where

import           Control.Monad.State
import           Data.List
import           Data.Semigroup
import qualified Data.Tree as T
  

-- Left and right refer to positions before the braiding operation
-- TODO: Separate into interior vertex and puncture types
data Vertex = Main | LeftPuncture | RightPuncture | Midpoint Edge | Contraction Edge
  deriving (Show, Eq)


-- TODO: rename to BasicEdge and make Edge = State TwoComplex BasicEdge
-- Orientations of initial edges are given by arrows in the figures in the paper
data Edge =
  -- initial edges
  LeftLoop | RightLoop | LeftLeg | RightLeg

  -- result of adding coev vertex (--(e)--> (coev e) --(e)-->
  | FirstHalf Edge | SecondHalf Edge

  -- connects the start of the two edges with a 1 in the disk
  | Connector Edge Edge Disk

   -- stick together parallel edges
  | TensorE Edge Edge

  -- don't use this constructor except to pattern match, use "rev" instead
  | Reverse Edge  
          deriving (Show, Eq)


data Disk =
  -- initial disks
  Outside | LeftDisk | RightDisk

  -- Edge should be of type Connect
  | Cut Edge          
          deriving (Show, Eq)


data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving (Eq, Show)


toDataTree :: Tree a -> T.Tree (Maybe a)
toDataTree (Leaf x) = T.Node (Just x) []
toDataTree (Node x y) = T.Node Nothing [toDataTree x, toDataTree y]

pprint :: (Show a) => Tree a -> IO ()
pprint = putStr . T.drawTree . fmap show . toDataTree


data TwoComplex = TwoComplex
                  { vertices      :: [Vertex]
                  , edges         :: [Edge]
                  , disks         :: [Disk]

                  -- The edges returned by perimeter should
                  -- form a cycle (the end point of an edge should be the
                  -- the starting point of the next edges).  Additionally,
                  -- the edges should either lie in the edges of the
                  -- TwoComplex or be the reverse of such an edge.
                  , perimeter     :: Disk -> [Edge]       

                  , imageVertex    :: Vertex -> Vertex     -- image under contractions
                  , morphismLabel :: Vertex -> Morphism   -- TODO: Change to Tree based on tensor structure
                  , edgeTree      :: Vertex -> Tree Edge  -- outgoing orientation
                  }

-- Monadic versions of methods

edgeTreeM :: Vertex -> State TwoComplex (Tree Edge)
edgeTreeM v = state $ \tc -> (edgeTree tc v, tc)


data Object = G | H | K | L
            | One
            | Star Object  -- Don't use this constructor except to pattern match, use "star" instead
            | TensorO Object Object
            deriving (Show)
                                                           
data Morphism = Phi
              | Id Object
              | Lambda Object -- 1 V -> V
              | LambdaI Object
              | Rho Object    -- V 1 -> V
              | RhoI Object
              | Alpha Object Object Object -- associator (xy)z = x(yz)
              | AlphaI Object Object Object -- inverse associator
              | Coev Object   -- currently right coev
              | Ev Object     -- right ev
              | TensorM Morphism Morphism
              | PivotalJ Object -- X -> X**
              | PivotalJI Object -- X** -> X
              | Compose Morphism Morphism
              deriving (Show)

-- domain :: Morphism -> Object
-- domain Phi = 

-- composable
instance Semigroup Morphism where
  a <> b = Compose a b


rev :: Edge -> Edge
rev (Reverse e) = e
rev (TensorE a b) = TensorE (rev a) (rev b)
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
  Connector e1 e2 _ -> [initialStart e1, initialStart e2]
  TensorE e1 _ -> initialEndpoints e1
  Reverse e -> reverse (initialEndpoints e)

initialStart :: Edge -> Vertex
initialStart e = (initialEndpoints e) !! 0

initialEnd :: Edge -> Vertex
initialEnd e = (initialEndpoints e) !! 1

-- TODO: maybe put this into TwoComplex, to make parallel with perimeter
--       also, eliminate "image"
endpoints :: Edge -> TwoComplex -> [Vertex]
endpoints e tc = map (imageVertex tc) (initialEndpoints e)

start :: Edge -> TwoComplex -> Vertex
start e tc = (endpoints e tc) !! 0

end :: Edge -> TwoComplex -> Vertex
end e tc = (endpoints e tc) !! 1


objectLabel :: Edge -> Object
objectLabel LeftLoop = G
objectLabel LeftLeg = H
objectLabel RightLoop = K
objectLabel RightLeg = L
objectLabel (FirstHalf e) = objectLabel e
objectLabel (SecondHalf e) = objectLabel e
objectLabel (Connector _ _ _) = One
objectLabel (TensorE e1 e2) = TensorO (objectLabel e1) (objectLabel e2)
objectLabel (Reverse e)  = star (objectLabel e)


treeLabel :: Tree Edge -> Object
treeLabel (Leaf e) = objectLabel e
treeLabel (Node x y) = TensorO (treeLabel x) (treeLabel y)


-- reverseEdge :: Edge -> State TwoComplex Edge
-- reverseEdge e0 = state $ \tc ->
--   (rev e0
--   , tc
--        { edges = [rev e0] ++ [e | e <- edges tc
--                                     , e /= e0] })

flatten :: Tree a -> [a]
flatten (Leaf x) = [x] 
flatten (Node x y) = (flatten x) ++ (flatten y)

replace :: (Eq a) => Tree a -> Tree a -> Tree a -> Tree a
replace subTree1 subTree2 bigTree = 
  if bigTree == subTree1
  then subTree2
  else case bigTree of
    Leaf x  -> Leaf x
    Node x y -> Node (replace subTree1 subTree2 x)
                (replace subTree1 subTree2 y)

associateL :: Vertex -> Tree Edge -> State TwoComplex (Tree Edge)
associateL v0 subTree@(Node x yz) =
  case yz of
    Node y z ->
      let newSubTree = (Node (Node x y) z) in
        state $ \tc ->
        (newSubTree, 
         tc
         { edgeTree = \v ->
             if v == v0
             then replace subTree newSubTree $ edgeTree tc v
             else edgeTree tc v
                  
         , morphismLabel = \v ->
             if v == v0
             then Compose (AlphaI (treeLabel x) (treeLabel y) (treeLabel z))
                  (morphismLabel tc v)
             else morphismLabel tc v
         }
        )

associateR :: Vertex -> Tree Edge -> State TwoComplex (Tree Edge)
associateR v0 subTree@(Node xy z) =
  case xy of
    Node x y ->
      let newSubTree = (Node x (Node y z)) in
        state $ \tc ->
                  (newSubTree,
                   tc
                    { edgeTree = \v ->
                        if v == v0
                        then replace subTree newSubTree $ edgeTree tc v
                        else edgeTree tc v
                         
                    , morphismLabel = \v ->
                        if v == v0
                        then Compose (Alpha (treeLabel x) (treeLabel y) (treeLabel z))
                             (morphismLabel tc v)
                        else morphismLabel tc v
                    }
                  )



isolateHelperR :: Vertex ->  TwoComplex -> TwoComplex
isolateHelperR v tc =
  let t = edgeTree tc v in
    case t of
      Node _ (Leaf x) -> tc
      Node _ (Node x y) -> isolateHelperR v $ execState (associateL v t) tc
  
isolateHelperL :: Vertex ->  TwoComplex -> TwoComplex
isolateHelperL v tc =
  let t = edgeTree tc v in
    case t of
      Node (Leaf x) _ -> tc
      Node (Node x y) _ -> isolateHelperL v $ execState (associateR v t) tc
  
     
-- Turns the far right leaf into a depth one leaf  
isolateR :: Vertex -> State TwoComplex ()
isolateR v0 = state $ \tc ->
  ((), isolateHelperR v0 tc)

isolateL :: Vertex -> State TwoComplex ()
isolateL v0 = state $ \tc ->
  ((), isolateHelperL v0 tc)



swap :: Tree a -> Tree a
swap (Node x y) = Node y x

zRotate :: Vertex -> State TwoComplex ()
zRotate v0 =
  isolateR v0 
  >> ( state $ \tc ->
  ((), tc
       { edgeTree = \v ->
           (
             if v == v0
             then swap 
             else id
           )
           $ edgeTree tc v
        
       ,  morphismLabel = \v ->
           if v == v0 
           then case (edgeTree tc v0) of
             Node y (Leaf x) ->
               let
                 xl = objectLabel x
                 yl = treeLabel y
               in
                 ((Id xl) `TensorM`  (Rho yl))
                 <> ((Id xl) `TensorM` ((Id yl) `TensorM` (Ev $ star xl))) -- X (Y 1)
                 <> ((Id xl) `TensorM` (Alpha yl xl (star xl))) -- X (Y (X *X))
                 <> ((Id xl) `TensorM` ((morphismLabel tc v) `TensorM` (Id $ star xl))) -- X 1 *X -> X ((Y X) *X)
                 <> ((PivotalJI xl) `TensorM` (LambdaI $ star xl))       -- **X *X -> X (1 *X)
                 <> (Coev $ star xl)  -- 1 -> **X *X
           else morphismLabel tc v
     }
  )
  )


rotateToEndHelper :: Edge -> Vertex -> State TwoComplex ()
rotateToEndHelper e0 v0 = state $ \tc ->
  ((),
   let
     es = flatten $ edgeTree tc v0
   in
     if es !! (length es - 1) == e0
     then tc
     else  execState (zRotate v0 >> rotateToEnd e0 v0) tc
  )
  
rotateToEnd e0 v0 = rotateToEndHelper e0 v0 >> isolateR v0

elemT u = (elem u) . flatten 

minimalSuperTree :: (Eq a) => a -> a -> Tree a -> Tree a
minimalSuperTree a1 a2 t@(Node x y) 
  | a1 `elemT` x && a2 `elemT` x = minimalSuperTree a1 a2 x
  | a1 `elemT` y && a2 `elemT` y = minimalSuperTree a1 a2 y
  | otherwise                    = t


-- Easy optimization: calculate t from previous t
isolate2Helper ::  Edge -> Edge -> Vertex -> TwoComplex -> TwoComplex
isolate2Helper e1 e2 v0 tc0 =
  let
    t = minimalSuperTree e1 e2 (edgeTree tc0 v0)
  in
    case t of
      Node x y -> 
        case x of
          Node x1 x2 -> isolate2Helper e1 e2 v0 $ execState (associateR v0 t) tc0
          Leaf x0 -> case y of
              Node y1 y2 -> isolate2Helper e1 e2 v0 $ execState (associateL v0 t) tc0
              Leaf y0 -> tc0

-- Put (rev) e1 and e2 on same node
-- TODO: infer Edge Tree argument
isolate2 :: Edge -> Edge -> Vertex  -> State TwoComplex ()
isolate2 e1 e2 v0 = state $ \tc0 ->
  let
    firstEdge = (flatten $ edgeTree tc0 v0) !! 0
    tc1 = if (e2 == firstEdge)
          then execState (zRotate v0) tc0
          else tc0
  in   
    ((), isolate2Helper e1 e2 v0 tc1)


-- The disk's perimeter should only have two edges
tensor :: Disk -> State TwoComplex Edge
tensor d0 =
  state $ \tc0 ->
  let
    e1 = (perimeter tc0 d0) !! 0
    e2 = rev ((perimeter tc0 d0) !! 1)
    v0 = (endpoints e1 tc0) !! 0
    v1 = (endpoints e1 tc0) !! 1
    product = TensorE e1 e2
    edgeImage e = case () of
      _ | e `elem` [e1, e2] -> product
        | e `elem` [rev e1, rev e2] -> rev product
        | otherwise -> e


    tc =  execState (isolate2 e1 e2 v0
                     >> isolate2 (rev e2) (rev e1) v1)
         tc0
  in
    ( product
    , tc
      { edges = map edgeImage (edges tc)
      , perimeter = (map edgeImage) . (perimeter tc)
      , edgeTree =  (replace (Node (Leaf e1) (Leaf e2)) (Leaf product))
                    . (replace (Node (Leaf $ rev e2) (Leaf $ rev e1)) (Leaf $ rev product))
                    . (edgeTree tc)
      }
    )


contract :: Edge -> State TwoComplex Vertex
contract contractedEdge  = state $ \tc ->
  let
    v0 = (endpoints contractedEdge tc) !! 0
    v1 = (endpoints contractedEdge tc) !! 1
    composition = Contraction contractedEdge
    newTC = execState (
      do 
        rotateToEnd contractedEdge v0
        rotateToEnd (rev contractedEdge) v1
        zRotate v1  
        isolateL v1
      ) tc
  in
  (composition, newTC
                { vertices = [composition] ++
                             [v | v <- vertices tc
                                , not $ v `elem` (endpoints contractedEdge tc)]
                , edges = [e | e <- edges tc
                             , e /= contractedEdge
                             , e /= rev contractedEdge]
                , imageVertex = (\v -> if (v `elem` (endpoints contractedEdge tc))
                                 then composition
                                 else v
                          ) . (imageVertex tc)
                , morphismLabel = (\v -> if (v == composition) 
                                         then  Compose (Ev $ objectLabel contractedEdge)
                                               (TensorM (morphismLabel tc (start contractedEdge tc))
                                                (morphismLabel tc (end contractedEdge tc)))
                                         else morphismLabel tc v )
                , edgeTree = \v ->
                    if v == composition
                    then Node (edgeTree tc $ start contractedEdge tc)
                         (edgeTree tc $ end contractedEdge tc)
                    else edgeTree tc v
                , perimeter = \d -> [e | e <- perimeter tc d
                                       , e /= contractedEdge
                                       , e /= rev contractedEdge
                                       ]
                }
  )


-- Connect the starting point of the first edge to that of the second
-- through the disk
-- The edges e1 and e2 should be elements of perimeter d.
connect :: Edge -> Edge -> Disk -> State TwoComplex Edge
connect e1 e2 d = state $ \tc -> 
  let connection = Connector e1 e2 d in
  ( connection
  , tc
      { edges = [connection] ++ edges tc
      , disks = [Cut connection, Cut $ rev connection]
                ++ [d2 | d2 <- disks tc
                       , d2 /= d]
      , edgeTree = \v -> case () of
        _ | v == start e1 tc -> replace (Leaf e1)
                                (Node (Leaf e1) (Leaf $ rev connection))
                                $ edgeTree tc v
          | v == start e2 tc -> replace (Leaf e2)
                                (Node (Leaf e2) (Leaf $ connection))
                                $ edgeTree tc v
          | otherwise        -> edgeTree tc v

      , morphismLabel = \v -> case () of
        _ | v == start e1 tc -> (RhoI $ objectLabel e1) <> morphismLabel tc v
          | v == start e2 tc -> (RhoI $ objectLabel e2) <> morphismLabel tc v
          | otherwise        -> morphismLabel tc v
          

      , perimeter = \d0 -> case () of
          _ | d0 == Cut connection -> [connection] ++
              (takeWhile (/= e1) $ dropWhile (/= e2) $ cycle $ perimeter tc d)
            | d0 == Cut (rev connection) -> [rev connection] ++
              (takeWhile (/= e2) $ dropWhile  (/= e1) $ cycle $ perimeter tc d)
            | otherwise -> perimeter tc d0
      }
  )
        
addCoev :: Edge -> State TwoComplex (Vertex, Edge, Edge)
addCoev e = state $ \tc ->
  let mp  = Midpoint e
      fh = FirstHalf e
      sh = SecondHalf e in
  ((mp, fh, sh), tc 
                { vertices =  [mp] ++ vertices tc
                , edges = [fh, sh] ++ [f | f <- edges tc
                                         , f /= e
                                         , f /= rev e]
                , morphismLabel = \v -> if v == mp
                                        then Coev $ objectLabel e
                                        else morphismLabel tc v
                , edgeTree = \v -> if v == mp
                                   then Node (Leaf $ rev $ FirstHalf e) (Leaf $ SecondHalf e)
                                   else edgeTree tc v
                , perimeter =  flip (>>=) (\es ->
                                             if es == [e]
                                             then [fh, sh]
                                             else if es == [rev e]
                                             then [rev sh, rev fh]
                                             else es
                                          ) . (map return) . perimeter tc
                }

  )

-- perimeter before contractions
initialPerimeter :: Disk -> [Edge]
initialPerimeter Outside    = [LeftLoop, RightLoop]
initialPerimeter LeftDisk   = [Reverse LeftLoop, LeftLeg, Reverse LeftLeg]
initialPerimeter RightDisk  = [Reverse RightLoop, RightLeg, Reverse RightLeg]


-- tcX corresponds to figure number X from the paper
initialTC = TwoComplex { vertices = [Main, LeftPuncture, RightPuncture]
                       , edges    = [LeftLoop, RightLoop, LeftLeg, RightLeg]
                       , disks    = [Outside, LeftDisk, RightDisk]
                       , imageVertex    = id
                       , perimeter = initialPerimeter
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

            
-- TODO: Linear logic/monadify these variables
slide = do
  (top1,l1,r1) <- addCoev LeftLoop
  (top2,l2,r2) <- addCoev LeftLeg
  (top3,r13,l3) <- addCoev r1
  (top4,l4,r4) <- addCoev RightLoop
  e1 <- connect (rev l1) r2 LeftDisk
  e2 <- connect (rev l2) (rev r13) (Cut $ e1)
  e3 <- connect l3 r4 Outside
  contract e1
  contract e2
  contract e3
  -- tensor (Cut $ rev e1)
  -- tensor (Cut $ rev e2)
  -- tensor (Cut $ rev e3)
  -- contract r4
  return ()

finalTC = execState slide initialTC


-- PASS
-- testTC = execState (isolate2 LeftLoop LeftLeg Main) initialTC
-- pprint $ edgeTree testTC Main

-- PASS
-- testTC2 =  execState (isolate2 (rev RightLoop) LeftLoop Main) initialTC
-- testTC2 = execState  (isolate2 (rev RightLoop) LeftLoop Main (edgeTree initialTC Main)) initialTC 
-- p =  pprint $ edgeTree testTC2 Main

-- PASS
-- testTC3 = execState  (zRotate Main) initialTC

--FAIL
test = execState (rotateToEnd LeftLeg Main) initialTC
p =  pprint $ edgeTree test Main

-- test = execState (
--   do
--     zRotate Main
--     zRotate Main
--     zRotate Main
--     isolateR Main
--   )
--   initialTC
-- p =  pprint $ edgeTree test Main


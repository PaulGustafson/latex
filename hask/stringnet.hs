-- Encode as marked CW-complex.
-- TODO:
-- Code up the local relations
-- Consider using simplicial complex library
-- Make better types for Vertex and Edge 

data InitialVertex = Main | LeftPuncture | RightPuncture deriving (Enum, Bounded, Show)

data InitialEdge = LeftLoop | RightLoop | LeftLeg | RightLeg deriving (Enum, Bounded, Show)

data InitialDisk = OutsideDisk | LeftDisk | RightDisk  deriving (Enum, Bounded, Show)

data AObject = X InitialEdge | One | Dual AObject | ObTensor AObject AObject  deriving (Show)

data AMorphism = Phi InitialVertex | Id AObject | Coev AObject | Ev AObject | MorTensor AMorphism AMorphism deriving (Show)

-- TODO: Add a name String
data Vertex = Vertex
              { vertexId       :: Int
              ,  vertexName :: String
              }  deriving (Eq)

data Edge = Edge
            { edgeId     :: Int
            } deriving (Eq)

data Disk = Disk
            { diskId     :: Int
            } deriving (Eq)

nextId :: [Int] -> Int
nextId xs = 1 + maximum xs

-- Should I make a dependent type of CW complexes with dimension as a parameter?
data ZeroComplex = ZeroComplex { zcVertices :: [Vertex] }

-- Consider changing this to a map Fin n -> Edge
data OneComplex = OneComplex
                  { ocZeroSkeleton   :: ZeroComplex
                  ,  ocEdges             :: [Edge]
                  ,  ocZeroBoundary :: Edge -> [Vertex]
                  }

data Orientation = Plus | Minus

type OrientedEdge = (Edge, Orientation)

oeBoundary :: OrientedEdge -> [Vertex]
oeBoundary (e, Plus) = zeroBoundary e
oeBoundary (e, Minus) = reverse zeroBoundary e

data TwoComplex = TwoComplex
                  { tcOneSkeleton  :: OneComplex
                  ,  tcDisks             :: Disk
                  ,  tcOneBoundary :: Disk -> [OrientedEdge] 
                  }

validOneBoundary :: [(OrientedEdge)] -> Bool
validOneBoundary [] = False
validOneBoundary es = and $ zipWith (==)  [(oeBoundary e) !! 1 | e <- es] [(oeBoundary e) !! 0 | e <- tail $ cycle es]

validTwoComplex :: TwoComplex -> Bool
validTwoComplex tc = and $ map validOneBoundary $ disks tc

data Stringnet = Stringnet
                 { twoComplex :: TwoComplex
                 , edgeLabel :: Edge -> AObject
                 , vertexLabel :: Vertex -> AMorphism
                 }

disks :: Stringnet -> [Disk]
disks sn = tcDisks . twoComplex

edges :: Stringnet ->  [Edge]
edges sn = ocEdges . tcOneSkeleton . twoComplex

vertices :: Stringnet -> [Vertex]
vertices sn = zcVertices . ocZeroSkeleton . tcOneSkeleton . twoComplex

-- TODO: Make sure vertex labels agree with edge labels
-- validEdgeLabelling :: Stringnet -> Bool

-- validStringnet :: Stringnet -> Bool
-- validStringnet (tc@(oc, _), aos, label) = validTwoComplex tc && validEdgeLabelling oc aos


-- initial conditions

initialZeroBoundary :: InitialEdge -> [InitialVertex]
initialZeroBoundary LeftLoop   = [Main, Main]
initialZeroBoundary RightLoop = [Main, Main]
initialZeroBoundary LeftLeg     = [Main, LeftPuncture]
initialZeroBoundary RightLeg   = [Main, RightPuncture]

initialOneBoundary :: InitialDisk -> [(InitialEdge, Orientation]
initialOneBoundary OutsideDisk = [(LeftLoop, Plus), (RightLoop, Plus)]
initialOneBoundary LeftDisk       = [(LeftLoop, Minus), (LeftLeg, Plus), (LeftLeg, Minus)]
initialOneBoundary RightDisk     = [(RightLoop, Minus), (RightLeg, Plus), (RightLeg, Minus)]

vertexFromIV :: InitialVertex -> Vertex
vertexFromIV iv = Vertex
                { vertexId = fromEnum iv
                ,  vertexName = show iv
                }

ivFromVertex :: Vertex -> InitialVertex
ivFromVertex v = toEnum vertexId v

ieFromEdge :: Edge -> InitialEdge
ieFromEdge e = toEnum vertexId e

diskFromID :: Disk -> InitialDisk
diskFromID d = toEnum diskId d

edgeFromIE :: InitialEdge -> Edge
edgeFromIE ie = Edge { edgeId = fromEnum ie }

initialVertices = [(minBound :: InitialVertex) ..]
initialEdges = [(minBound :: InitialEdge) ..]
initialDisks = [(minBound :: Disks) ..]

initialZeroComplex :: ZeroComplex
initialZeroComplex = map vertexFromIV initialVertices

initialOneComplex :: OneComplex
initialOneComplex = OneComplex
                  { zeroSkeleton  = initialZeroComplex
                  ,  edges             :: map edgeFromIE initialEdges
                  ,  zeroBoundary :: map vertexFromIV $ initialZeroBoundary toEnum
                  }

initialTwoComplex :: TwoComplex
initialTwoComplex = TwoComplex
                    { oneSkeleton = initialOneComplex
                    ,  disks = initialDisks
                    ,  oneBoundary = map f $ initialOneBoundary diskFromID
                                     where f(ie, _) = (edgeFromIE ie, _)
                    }

initialStringnet :: Stringnet
initialStringnet = Stringnet
                   { twoComplex = initialTwoComplex
                   , edgeLabel     = X . ieFromEdge
                   , vertexLabel = Phi . ivFromVertex
                   }


-- local relations

-- Pushes the starting vertex of the oriented edge into the end
contract :: Stringnet -> OrientedEdge -> Stringnet
contract sn oe = Stringnet
  let tc = twoComplex sn in
    { twoComplex = tc {
        tcOneSkeleton = [v | v <- vertices sn, v != oeBoundary !! 0]
                       ocEdges = [e | e <- edges sn,  e != fst oe]
                       ocZeroBoundary = ocZeroBoundary 
                                                         }
                                              
                               , attachingMaps = d
                               }                               
                , edgeLabel  = edgeLabel sn
                , vertexLabel = --FIXME vertexLabel sn
                }
                
                 


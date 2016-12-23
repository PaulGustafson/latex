-- Encode as marked CW-complex.
-- TODO:
-- Code up the local relations
-- Consider using simplicial complex library
-- Make better types for Vertex and Edge 

data InitialVertex = Main | LeftPuncture | RightPuncture deriving (Enum, Bounded, Show)

data InitialEdge = LeftLoop | RightLoop | LeftLeg | RightLeg deriving (Enum, Bounded, Show)

data InitialDisk = OutsideDisk | LeftDisk | RightDisk  deriving (Enum, Bounded, Show)

data AObject = X InitialEdge | One | Dual AObject | ObTensor AObject AObject

data AMorphism = Phi InitialVertex | Id AObject | Coev AObject | Ev AObject | MorTensor AMorphism AMorphism

-- TODO: Change to newtype and fix the "toEnum" stuff
-- TODO: Add a name String
data Vertex = Vertex
              { vertexID       :: Int
              ,  vertexName :: String
              }

data Edge = Edge
            { edgeID     :: Int
            }

data Disk = Disk
            { diskID     :: Int
            }

nextID :: [Int] -> Int
nextID xs = 1 + maximum xs

-- Should I make a dependent type of CW complexes with dimension as a parameter?
data ZeroComplex = ZeroComplex { vertices :: [Vertex] }

-- Consider changing this to a map Fin n -> Edge
data OneComplex = OneComplex
                  { zeroSkeleton   :: ZeroComplex
                  ,  edges             :: [Edge]
                  ,  zeroBoundary :: Edge -> [Vertex]
                  }

data Orientation = Plus | Minus

type OrientedEdge = (Edge, Orientation)

oeBoundary :: OrientedEdge -> [Vertex]
oeBoundary (e, Plus) = zeroBoundary e
oeBoundary (e, Minus) = reverse zeroBoundary e

data TwoComplex = TwoComplex
                  { oneSkeleton :: OneComplex
                  ,  disks             :: Disk
                  ,  oneBoundary :: Disk -> [OrientedEdge] 
                  }

validOneBoundary :: [(OrientedEdge)] -> Bool
validOneBoundary [] = False
validOneBoundary es = and $ zipWith (==)  [(oeBoundary e) !! 1 | e <- es] [(oeBoundary e) !! 0 | e <- tail $ cycle es]

validTwoComplex :: TwoComplex -> Bool
validTwoComplex tc = and $ map validOneBoundary $ disks tc

-- label the complex
data Stringnet = Stringnet
                 { twoComplex :: TwoComplex
                 , edgeLabel :: Edge -> AObject
                 , vertexLabel :: Vertex -> AMorphism
                 }

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

ivToVertex :: InitialVertex -> Vertex
ivToVertex iv = Vertex
                { vertexID = fromEnum iv
                ,  vertexName = show iv
                }

vertexToInitial :: Vertex -> InitialVertex
vertexToInitial v = toEnum vertexID v

edgeToInitial :: Edge -> InitialEdge
edgeToInitial e = toEnum vertexID e

diskToInitial :: Disk -> InitialDisk
diskToInitial d = toEnum diskID d

ieToEdge :: InitialEdge -> Edge
ieToEdge ie = Edge { edgeID = fromEnum ie }

initialVertices = [(minBound :: InitialVertex) ..]
initialEdges = [(minBound :: InitialEdge) ..]
initialDisks = [(minBound :: Disks) ..]

initialZeroComplex :: ZeroComplex
initialZeroComplex = map ivToVertex initialVertices

initialOneComplex :: OneComplex
initialOneComplex = OneComplex
                  { zeroSkeleton  = initialZeroComplex
                  ,  edges             :: map ieToEdge initialEdges
                  ,  zeroBoundary :: map ivToVertex $ initialZeroBoundary toEnum
                  }

initialTwoComplex :: TwoComplex
initialTwoComplex = TwoComplex
                    { oneSkeleton = initialOneComplex
                    ,  disks = initialDisks
                    ,  oneBoundary = map f $ initialOneBoundary diskToInitial
                                     where f(ie, _) = (ieToEdge ie, _)
                    }

initialStringnet :: Stringnet
initialStringnet = Stringnet
                   { twoComplex = initialTwoComplex
                   , edgeLabel     = X . edgeToInitial
                   , vertexLabel = Phi . vertexToInitial
                   }

contract :: Stringnet -> Edge -> Stringnet
contract sn i = Stringnet
                { twoComplex = TwoComplex
                               { oneComplex = oneComplex twoComplex sn
                               , attachingMaps = d
                               }                               
                , edgeLabel  = edgeLabel sn
                , vertexLabel = --FIXME vertexLabel sn
                }
                
                 


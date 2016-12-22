-- Encode as marked CW-complex.


data InitialVertex = Main | LeftPuncture | RightPuncture deriving (Enum)

data InitialEdge = LeftLoop | RightLoop | LeftLeg | RightLeg deriving (Enum)

data AObject  =  G InitialEdge | One | Inverse AObject | Product AObject AObject

data AMorphism = Phi InitialVertex | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

-- TODO: Change to newtype and fix the "toEnum" stuff
type Vertex = Int

type EdgeIndex = Int  -- unique id for each edge, not the array index in OneComplex

type Edge = ([Vertex], EdgeIndex)

type Disk = [Edge] -- a cycle, i.e. snd edge1 = fst edge2 ..

validDisk :: Disk -> Bool
validDisk [] = False
validDisk es = and $ zipWith (==)  [vs !! 1 | (vs, _) <- es] [vs !! 0 | (vs, _) <- tail $ cycle es]

-- Consider changing this to a map Fin n -> Edge
type OneComplex = [Edge]

data Orientation = Plus | Minus

type AttachingMap = [(EdgeIndex, Orientation)] 

toDisk :: OneComplex -> AttachingMap -> Disk
toDisk oc am = [oc !! i | (i,o) <- am]

validAttachingMap :: OneComplex -> AttachingMap -> Bool
validAttachingMap oc am = (and [ i < length oc | (i,o) <- am]) && (validDisk $ toDisk oc am)

data TwoComplex = TwoComplex { oneComplex :: OneComplex
                             , attachingMaps :: [AttachingMap]
                             }

validTwoComplex :: TwoComplex -> Bool
validTwoComplex tc = and $ map (validAttachingMap $ oneComplex tc) $ attachingMaps tc

-- label the complex
-- TODO: Make the label pattern-matching exhaustive 
data Stringnet = Stringnet {twoComplex :: TwoComplex
                           , edgeLabel :: Edge -> AObject
                           , vertexLabel :: Vertex -> AMorphism
                           }

-- TODO
-- validEdgeLabelling :: Stringnet -> Bool

-- validStringnet :: Stringnet -> Bool
-- validStringnet (tc@(oc, _), aos, label) = validTwoComplex tc && validEdgeLabelling oc aos


-- more initial conditions
vertices :: InitialEdge -> [Vertex]
vertices LeftLoop  = map fromEnum [Main, Main]
vertices RightLoop = map fromEnum [Main, Main]
vertices LeftLeg   = map fromEnum [Main, LeftPuncture]
vertices RightLeg  = map fromEnum [Main, RightPuncture]


initialOneComplex :: OneComplex
initialOneComplex = [(vertices ie, fromEnum ie) | ie <- [LeftLoop .. RightLeg]]

initialTwoComplex :: TwoComplex
initialTwoComplex = TwoComplex initialOneComplex $ map (map f)
                     [[(LeftLoop, Plus), (RightLoop, Plus)]
                     ,[(LeftLoop, Minus), (LeftLeg, Plus), (LeftLeg, Minus)]
                     ,[(RightLoop, Minus), (RightLeg, Plus), (RightLeg, Minus)]
                     ]
                    where f (a, b) = (fromEnum a, b)

initialStringnet :: Stringnet
initialStringnet = Stringnet { twoComplex  = initialTwoComplex
                             , edgeLabel   = G . toEnum . snd
                             , vertexLabel = Phi . toEnum
                             }

contract :: Stringnet -> EdgeIndex -> Stringnet
contract sn i = Stringnet { twoComplex = TwoComplex { oneComplex = oneComplex twoComplex sn
                                                    , attachingMaps = d
                                                    }                               
                          , edgeLabel  = edgeLabel sn
                          , vertexLabel = --FIXME vertexLabel sn
                          }
                
                 


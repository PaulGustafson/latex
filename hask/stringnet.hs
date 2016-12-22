-- Encode as marked CW-complex.

data AObject  =  G | H | K | L | One | Inverse AObject | Product AObject AObject

data AMorphism = Phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

type Vertex = Integer

type EdgeIndex = Integer  -- unique id for each edge, not the array index in OneComplex

-- change into a record?
type Edge = ([Vertex], EdgeIndex)

type Disk = [Edge] -- a cycle, i.e. snd edge1 = fst edge2 ..

validDisk :: Disk -> Bool
validDisk [] = False
validDisk es = and $ zipWith (==)  [vs !! 1 | (vs, _) <- es] [vs !! 0 | (vs, _) <- tail $ cycles es]

-- Consider changing this to a map Fin n -> Edge
data OneComplex = [Edge]

data Orientation = Plus | Minus

type AttachingMap = [(EdgeIndex, Orientation)] 

toDisk :: OneComplex -> AttachingMap -> Disk
toDisk oc am = [oc !! i | (i,o) <- am]

validAttachingMap :: OneComplex -> AttachingMap -> Bool
validAttachingMap oc am = (and [ i < length oc | (i,o) <- am]) && validDisk $ toDisk oc am

data TwoComplex = TwoComplex { oneComplex :: OneComplex,
                             , attachingMaps :: [AttachingMap]
                             }

validTwoComplex :: TwoComplex -> Bool
validTwoComplex tc = and $ map (validAttachingMap $ oneCompex tc) $ attachingMaps tc

-- label the complex
-- TODO: Make the pattern-matching exhaustive (get rid of Maybe)
data Stringnet = Stringnet {twoComplex :: TwoComplex
                           , edgeLabel :: Edge -> Maybe AObject,
                           , vertexLabel :: Vertex -> Maybe AMorphism
                           }

-- TODO
-- validEdgeLabelling :: Stringnet -> Bool

-- validStringnet :: Stringnet -> Bool
-- validStringnet (tc@(oc, _), aos, label) = validTwoComplex tc && validEdgeLabelling oc aos


-- initial conditions
data InitialVertex = Main | LeftPuncture | RightPuncture  deriving (Enum)

data InitialEdge = LeftLoop | RightLoop | LeftLeg | RightLeg deriving (Enum)

vertices :: InitialEdge -> [Vertex]
vertices LeftLoop  = map fromEnum [Main, Main]
vertices RightLoop = map fromEnum [Main, Main]
vertices LeftLeg   = map fromEnum [Main, LeftPuncture]
vertices RightLeg  = map fromEnum [Main, RightPuncture]


initialOneComplex :: OneComplex
initialOneComplex = [(vertices ie, fromEnum ie) | ie <- [LeftLoop .. RightLeg]]

initialTwoComplex :: TwoComplex
initialTwoComplex = (initialOneComplex, map (map (fromEnum, id))
                     [[(LeftLoop, Plus), (RightLoop, Plus)]
                     ,[(LeftLoop, Minus), (LeftLeg, Plus), (LeftLeg, Minus)]
                     ,[(RightLoop, Minus), (RightLeg, Plus), (RightLeg, Minus)]
                     ])

                    

                    
contract :: Stringnet -> EdgeIndex -> Stringnet
contract sn i = Stringnet


-- Encode as marked CW-complex.

type Vertex = Integer  -- Main | LeftPuncture | RightPuncture | Between Vertex Vertex

type Edge = (Vertex, Vertex)

data AObject  =  G | H | K | L | One | Inverse AObject | Product AObject AObject

data AMorphism = Phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

type Disk = [Edge] -- a cycle, i.e. snd edge1 = fst edge2 ..

validDisk :: Disk -> Bool
validDisk [] = False
validDisk d = and $ zipWith (==) (map snd d) (map fst $ tail $ cycle d)

data OneComplex = [Edge]

data Orientation = Plus | Minus

type EdgeIndex = Integer

type AttachingMap = [(EdgeIndex, Orientation)] 

toDisk :: OneComplex -> AttachingMap -> Disk
toDisk oc am = [oc !! i | (i,o) <- am]

validAttachingMap :: OneComplex -> AttachingMap -> Bool
validAttachingMap oc am = (and [ | (i,o) <- am,  i < length oc]) && validDisk $ toDisk oc am

type TwoComplex = (OneComplex, [AttachingMap] )

validTwoComplex :: TwoComplex -> Bool
validTwoComplex (oc, ams) = and $ map (validAttachingMap oc) ams

-- label the complex
type Stringnet = (TwoComplex, [AObject], Vertex -> AMorphism)

-- TODO
-- validEdgeLabelling :: Stringnet -> Bool

-- validStringnet :: Stringnet -> Bool
-- validStringnet (tc@(oc, _), aos, label) = validTwoComplex tc && validEdgeLabelling oc aos
  

type ColoredEdge = (AObject, Vertex, EdgeOrientation)

data EdgeTree =  Leaf ColoredEdge | Times EdgeTree EdgeTree

type VertexColoring = (AMorphism, EdgeTree) 

type Stringnet = Vertex ->  Maybe VertexColoring


                        


-- need isotopy information.  Encode as marked CW-complex.

data AObject  =  G | H | K | L | One | Inverse AObject | Product AObject AObject

data AMorphism = Phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

data Vertex = Main | LeftPuncture | RightPuncture | Between Vertex Vertex

type Edge = (Vertex, Vertex)

type Disk = [Edge] -- a cycle, i.e. snd edge1 = fst edge2 ..

validDisk :: Disk -> Bool
validDisk [] = False
validDisk d = foldl (&&) $ zipWith (==) (map snd d) (map fst $ tail $ cycle d)

data Graph = [Edge]  -- 

data EmbeddedGraph = [Disk]  -- How are the disks connected?

data EdgeOrientation = Out | In  

type ColoredEdge = (AObject, Vertex, EdgeOrientation)

data EdgeTree =  Leaf ColoredEdge | Times EdgeTree EdgeTree

type VertexColoring = (AMorphism, EdgeTree) 

type Stringnet = Vertex ->  Maybe VertexColoring


               
        
               
              
              

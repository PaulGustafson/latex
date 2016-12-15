

-- need isotopy information.  Encode as triangulation?

data AObject =  g | h | k | l | One | Inverse AObject | Product AObject AObject

type AMorphism = phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

data Vertex = Main | LeftHole | RightHole | Top1 | Top2 | Top3 | Top4

-- Multiple edges between two nodes are enumerated in
-- CCW order from the start node, starting at the eastward ray
data Edge = Edge { start :: Vertex
                 , end   :: Vertex
                 , nextEdge :: Maybe Edge
                 } deriving (Show)


data Stringnet = Stringnet { 
                            vLabel  :: Vertex -> AMorphism
                           , eLabel  :: Edge -> AObject
                           } 


-- replace the starting vertex with the composition, make the end vertex empty
compose :: Stringnet -> Edge -> Stringnet
compose (Stringnet vLabel eLabel) edge@(Edge composeStart end nextEdge) =  Stringnet vLabel2 eLabel2
  where vLabel2 composeStart = Prod (vLabel composeStart) (Prod (Ev $ Inverse $ eLabel edge) (vLabel end))
        vLabel2 end   = One
        vLabel2 other = vlabel other
        eLabel2 (Edge _ end _ )  = One  -- kill all edges into endNode 
        eLabel2 (Edge composeStart _ _) = eLabel 

               
        
               
              
              

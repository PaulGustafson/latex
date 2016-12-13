
data AObject =  g | h | k | l | One | Inverse AObject | Product AObject AObject

type AMorphism = phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism


type Vertex = Main | LeftHole | RightHole | Top1 | Top2 | Top3 | Top4

-- Multiple edges between two nodes are enumerated in
-- CCW order from the start node, starting at the eastward ray
data Edge = Edge { start :: Vertex
                 , end   :: Vertex
                 , index :: Integer
                 } deriving (Show)


data Stringnet = Stringnet { --outgoingEdges :: Vertex -> [Edge]
                            vLabel  :: Vertex -> AMorphism
                           , eLabel  :: Edge -> AObject
                           } 


-- replace the starting vertex with the composition, make the end vertex empty
-- Is there a way to intermediate pattern-match edge = (Edge start end index) ?
compose :: Stringnet -> Edge -> Stringnet
compose sn edge@(Edge start end index) =  Stringnet vLabel2 eLabel2
  where vLabel2 start = Prod (vLabel start) (Prod (Ev $ Inverse $ eLabel edge) (vLabel end))
        vLabel2 end   = One
        vlabel2 other = vlabel other
        eLabel2 (Edge start end _ ) = One
        elabel2 

               
        
               
              
              



-- need isotopy information.  Encode as triangulation?

data AObject =  g | h | k | l | One | Inverse AObject | Product AObject AObject

type AMorphism = phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

data Vertex = Main | LeftHole | RightHole | Top1 | Top2 | Top3 | Top4

data HalfEdge = { vertex    :: Vertex
                , nextEdge  :: Edge}

data Orientation = ZeroOne | OneZero

-- nextEdge is the next edge in CCW order around the specified endpoint
data Edge = Edge { halfEdges   :: [HalfEdge]
                   orientation :: Orientation
                 } 

data Stringnet = Stringnet { edges     :: [Edge]
                           , vertices  :: [Vertex]
                           , vLabel    :: Vertex -> AMorphism
                           , eLabel    :: Edge   -> AObject 
                           } 


-- replace the starting vertex with the composition, make the end vertex empty
-- cEdge stands for "contracted edge"
compose :: Stringnet -> Edge -> Stringnet
compose (Stringnet edges1 vertices1 vLabel1 eLabel1) cEdge@(Edge cStart cEnd _ _ ) =  Stringnet edges2 vertices2 vLabel2 eLabel2
  where
    edges2 = [e | e <- edges1, end e != cEnd, start e != cEnd]
                
    vLabel2 cStart = Just $ Prod (vLabel cStart cEnd) (Prod (Ev $ Inverse $ eLabel1 cEdge) (vLabel1 cEnd))
        vLabel2 cStart = Nothing
        vLabel2 other  = vlabel other
        eLabel2 Edge {end = cEnd}    = Nothing  -- kill all edges into endNode
        eLabel2 Edge {start = cEnd}  = Nothing
        eLabel2 Edge {nextEdgeAtStart = cEdge} = 
        eLabel2 other = eLabel other

               
        
               
              
              

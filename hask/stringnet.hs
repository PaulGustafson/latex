

-- need isotopy information.  Encode as triangulation?

data AObject =  g | h | k | l | One | Inverse AObject | Product AObject AObject

type AMorphism = phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

data Vertex = Main | LeftHole | RightHole | Top1 | Top2 | Top3 | Top4

data HalfEdge = { vertex    :: Vertex
                , nextEdge  :: Edge
                }

data Orientation = ZeroOne | OneZero

-- nextEdge is the next edge in CCW order around the specified endpoint
data Edge = Edge { halfEdges   :: [HalfEdge]
                   orientation :: Orientation
                 } 

data Stringnet = Stringnet { edges     :: [(Edge, AObject)]
                           , vertices  :: [(Vertex, AMorphism)]
                           } 

end :: Edge -> Vertex
end e = (halfEdges e) !! [if orientation e == ZeroOne then 1 else 0]


-- replace the starting vertex with the composition, make the end vertex empty
-- cEdge stands for "contracted edge"
compose :: Stringnet -> Edge -> Stringnet
compose sn cEdge =   Stringnet newEdges newVertices
               where  
                 newEdges = [e | e <- edges sn, not $ elem (end cEdge) $ map vertex (halfEdges e)] -- kill all edges touching the end node
                            ++ [ ]
                


    -- vLabel2 cStart = Just $ Prod (vLabel cStart cEnd) (Prod (Ev $ Inverse $ eLabel1 cEdge) (vLabel1 cEnd))
    --     vLabel2 cStart = Nothing
    --     vLabel2 other  = vlabel other
    --     eLabel2 Edge {end = cEnd}    = Nothing  -- kill all edges into endNode
    --     eLabel2 Edge {start = cEnd}  = Nothing
    --     eLabel2 Edge {nextEdgeAtStart = cEdge} = 
    --     eLabel2 other = eLabel other

               
        
               
              
              

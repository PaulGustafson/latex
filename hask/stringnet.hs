

-- need isotopy information.  Encode as triangulation?

data AObject =  g | h | k | l | One | Inverse AObject | Product AObject AObject

type AMorphism = phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

data Vertex = Main | LeftHole | RightHole | Top1 | Top2 | Top3 | Top4

data HalfEdge = { vertex           :: Vertex
                , nextHalfEdge     :: HalfEdge
                , oppositeHalfEdge :: HalfEdge
                }

type Edge = (HalfEdge, HalfEdge)
    

data Stringnet = Stringnet { edges     :: [(Edge, AObject)]
                           , vertices  :: [(Vertex, AMorphism)]
                           } 


start :: Edge -> Vertex
start = fst

end :: Edge -> Vertex
end = snd 


-- replace the starting vertex with the composition, make the end vertex empty
-- cEdge stands for "contracted edge"
compose :: Stringnet -> Edge -> Stringnet
compose sn cEdge =   Stringnet newEdges newVertices
               where
                 -- all edges touching the end node should touch the start node now
                 let endEdges =  [e | e <- edges sn, elem (end cEdge) $ map vertex (halfEdges e)] in --FIXME
                   newEdges = [e | e <- edges, not $ elem e endEdges]
                              ++ [Edge    | e <- endEdges,  
                


    -- vLabel2 cStart = Just $ Prod (vLabel cStart cEnd) (Prod (Ev $ Inverse $ eLabel1 cEdge) (vLabel1 cEnd))
    --     vLabel2 cStart = Nothing
    --     vLabel2 other  = vlabel other
    --     eLabel2 Edge {end = cEnd}    = Nothing  -- kill all edges into endNode
    --     eLabel2 Edge {start = cEnd}  = Nothing
    --     eLabel2 Edge {nextEdgeAtStart = cEdge} = 
    --     eLabel2 other = eLabel other

               
        
               
              
              

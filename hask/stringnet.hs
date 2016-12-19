

-- need isotopy information.  Encode as triangulation?

data AObject =  g | h | k | l | One | Inverse AObject | Product AObject AObject

type AMorphism = phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

data Vertex = Main | LeftPuncture | RightPuncture | Top1 | Top2 | Top3 | Top4

data HalfEdge = { vertex           :: Vertex
                , nextHalfEdge     :: HalfEdge
                , oppositeHalfEdge :: HalfEdge
                }

type Edge = (HalfEdge, HalfEdge)

type ColoredEdge = (Edge, AObject)

type ColoredVertex = (Vertex, AMorphism)
    
data Stringnet = Stringnet { coloredEdges     :: [ColoredEdge]
                           , coloredVertices  :: [ColoredVertex]
                           } 


start :: ColoredEdge -> Vertex
start = vertex . fst . fst

end :: ColoredEdge -> Vertex
end = vertex . snd . fst


-- calculates image of new edge 
composeHelper :: Stringnet -> ColoredEdge -> ColoredEdge -> ColoredEdge
composeHelper oldStringnet contractedEdge oldEdge = newEdge
  where
    -- all edges touching the end node should touch the start node now
    let startsWithCEnd = [ c | c <- coloredEdges oldStringnet,  end contractedEdge == start c]
        endsWithCEnd   = [ c | c <- coloredEdges oldStringnet,  end contractedEdge == end   c]
        touchesCEnd    = startsWithCEnd ++ endsWithCEnd
      in 
               

-- replace the starting vertex with the composition, make the end vertex empty
compose :: Stringnet -> ColoredEdge -> -> Stringnet
compose oldStringnet contractedEdge =  Stringnet newColoredEdges newColoredVertices
               where
                 -- all edges touching the end node should touch the start node now
                 let startsWithCEnd = [ c | c <- coloredEdges oldStringnet,  end contractedEdge == start c]
                     endsWithCEnd   = [ c | c <- coloredEdges oldStringnet,  end contractedEdge == end   c]
                     touchesCEnd    = startsWithCEnd ++ endsWithCEnd
                   in 
                  newEdges = [c | c <- coloredEdges oldStingnet, not $ c `elem` touchesCEnd]
                              ++ [ | (( <- startsWithCEnd,  ]
                


    -- vLabel2 cStart = Just $ Prod (vLabel cStart cEnd) (Prod (Ev $ Inverse $ eLabel1 cEdge) (vLabel1 cEnd))
    --     vLabel2 cStart = Nothing
    --     vLabel2 other  = vlabel other
    --     eLabel2 Edge {end = cEnd}    = Nothing  -- kill all edges into endNode
    --     eLabel2 Edge {start = cEnd}  = Nothing
    --     eLabel2 Edge {nextEdgeAtStart = cEdge} = 
    --     eLabel2 other = eLabel other

               
        
               
              
              

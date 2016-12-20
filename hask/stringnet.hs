
-- need isotopy information.  Encode as triangulation?

data AObject =  G | H | K | L | One | Inverse AObject | Product AObject AObject

data AMorphism = Phi | Id AObject | Coev AObject | Ev AObject | Prod AMorphism AMorphism

data Vertex = Main | LeftPuncture | RightPuncture | Top1 | Top2 | Top3 | Top4

data Edge = (Vertex, Vertex, FundamentalGroupoidElt) 

data EdgeOrientation = Out | In

type ColoredEdge = (AObject, Vertex, EdgeOrientation)

data EdgeTree =  Leaf ColoredEdge | Times EdgeTree EdgeTree

type VertexColoring = (AMorphism, EdgeTree) 

type Stringnet = Vertex ->  Maybe VertexColoring

-- How can I match up half-edges?  Adjacency matrix?

firstFrame :: StringNet
firstFrame Main = Just (Phi, (Leaf (G, Main, Out)) `Times` (Leaf H) `Times` (Leaf G)
                             `Times` (Leaf K) `Times` (Leaf L) `Times` (Leaf K))

-- replace the starting vertex with the composition, make the end vertex empty
-- compose :: Stringnet -> ColoredEdge -> -> Stringnet
-- compose oldStringnet contractedEdge =  Stringnet newColoredEdges newColoredVertices
--                where
--                  -- all edges touching the end node should touch the start node now
--                  let startsWithCEnd = [ c | c <- coloredEdges oldStringnet,  end contractedEdge == start c]
--                      endsWithCEnd   = [ c | c <- coloredEdges oldStringnet,  end contractedEdge == end   c]
--                      touchesCEnd    = startsWithCEnd ++ endsWithCEnd
--                    in 
--                   newEdges = [c | c <- coloredEdges oldStingnet, not $ c `elem` touchesCEnd]
--                               ++ [ | (( <- startsWithCEnd,  ]
                


    -- vLabel2 cStart = Just $ Prod (vLabel cStart cEnd) (Prod (Ev $ Inverse $ eLabel1 cEdge) (vLabel1 cEnd))
    --     vLabel2 cStart = Nothing
    --     vLabel2 other  = vlabel other
    --     eLabel2 Edge {end = cEnd}    = Nothing  -- kill all edges into endNode
    --     eLabel2 Edge {start = cEnd}  = Nothing
    --     eLabel2 Edge {nextEdgeAtStart = cEdge} = 
    --     eLabel2 other = eLabel other

               
        
               
              
              

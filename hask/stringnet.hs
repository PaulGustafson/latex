
type AObject = String

type AMorphism = String

data Vertex = Vertex { vertexLabel :: AMorphism
                     } deriving (Show)
  
data Edge = Edge { start :: Vertex
                 , end   :: Vertex
                 , edgeLabel :: AObject
                 } deriving (Show)

data Stringnet = StringNet { vertices :: [Vertex]
                           , edges    :: [Edges]
                           } deriving (Show)



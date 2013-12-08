data GeneralNode a = GeneralNode { 
    nodeActivity :: a          -- ^ current activation output level. Has to be updated manually (through evaluate)
}

type Node = GeneralNode Activity
newtype Activity = Activity { getActivity :: Double }

instance (Show a) => Show (GeneralNode a) where
   show = show . nodeActivity

instance Show Activity where
  show a = show . myround 100 $ getActivity a
         where myround s = (/s) . fromIntegral . round . (*s)

instance Functor GeneralNode where
  fmap f (GeneralNode a) = GeneralNode (f a)

class HasActivity n where
    evolve :: n -> n

data Network = Network {
  networkNodes :: Nodes,                      -- ^ List of all Nodes
  networkWeightedEdges :: WeightedEdges,      -- ^ List of all Edges
  networkArchitecture :: Architecture         -- ^ List of node counts for each layer
} deriving (Show)

getForwardNodes  = getConnectedNodesBy fst snd
getBackwardNodes = getConnectedNodesBy snd fst

getConnectedNodesBy :: (Edge -> NodeIndex) -> (Edge -> NodeIndex) -> Network -> NodeIndex -> [NodeIndex]
getConnectedNodesBy f g n i = map g . (filter ((==i) . f)) . map getEdgeFromWeightedEdge . networkWeightedEdges $ n

type Architecture = [Int]
type Layer = Network

type Nodes = [Node]
type WeightedEdges = [(Weight, Edge)]
getWeightFromWeightedEdge = fst
getEdgeFromWeightedEdge   = snd

type Edge = (NodeIndex, NodeIndex)
type NodeIndex = Int
type Weight = Double
type Threshold = Double
type Logit = Double

{-
instance Node SparseNode where
    evaluate n@(SparseNode a ps ws)
      | null ps = n                 -- allow for input nodes to let their activity be unaffected despite having no inputs
      | otherwise = SparseNode newactivity evps ws
      where newactivity = s . foldl f 0 $ zip evps ws
            s = sigmoid
            f acc ((SparseNode a _ _), w) = a * w + acc
            evps = map evaluate ps -- deepEvaluate predecessor nodes

heaviside x | x  > 0    = 1
            | x == 0    = 0.5
            | otherwise = 0

sigmoid x = 1 / (1 + (exp (-x)))
-}



--Architecture/ NetLib

-- | create fully connected multi layer network from a list of node counts per layer
createNetwork :: Architecture -> Network
createNetwork = foldl1 stackLayer . map createLayer

-- | create one layer network from number of nodes
createLayer :: Int -> Layer
createLayer n = Network nodes [] [n]
              where nodes = take n . repeat $ singletonNode 0.5

-- | singleton node 
singletonNode a = GeneralNode $ Activity a

-- | add a one layer network with full connectivity to last layer of old network
stackLayer :: Network -> Layer -> Network
stackLayer n@(Network ns wes a) l = Network (ns++newnodes) (wes++newedges) (a++[newnodecount])
  where newedges = [(1, (oldnodecount - lastlayernodecount + ip, oldnodecount + is)) | ip <- [0..lastlayernodecount-1], is <- [0..newnodecount-1]]
        newnodes = networkNodes l
        oldnodecount = length ns
        newnodecount = length newnodes
        lastlayernodecount = last a

networkNodeIndices = accumulate . networkArchitecture
networkEdgeIndices = accumulate . edgeCounts
accumulate = scanl (+) 0

edgeCounts = combinePairs (*) . networkArchitecture
combinePairs f xs = map (uncurry f) $ zip (init xs) (tail xs)



-- Test data

testnetwork = createNetwork [1, 2, 1]
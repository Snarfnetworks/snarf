type Activity = Double
type Weight = Double
type Threshold = Double
type Logit = Double

class Node n where
    evaluate :: n -> n
    logit :: n -> Logit

data SparseNode = SparseNode { 
      sparseNodeActivity :: Activity          -- ^ current activation output level. Has to be updated manually (through evaluate)
    , sparseNodePredecessors :: [SparseNode]  -- ^ input nodes
    , sparseNodeWeights :: [Weight]           -- ^ weights for input node activation levels
}-- deriving Show

-- | singleton node 
inputNode a = SparseNode a [] []

instance Show SparseNode where
   show (SparseNode a _ _) = show . myround 100 $ a
                           where myround s = (/s) . fromIntegral . round . (*s)

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


--Architecture/ NetLib

data Layer n = Layer [n] --TODO How can I restrict n to be instance of typeclass Node? A: newtype
    deriving (Show)

instance Functor Layer where
    fmap f (Layer n) = Layer $ map f n

-- Test data


-- nodes
n1 = SparseNode 0.0 (map inputNode [0.1, 0.4, 0.3]) [2, -3, 1]
n2 = inputNode 0.7
n3 = inputNode 0.1
n4 = inputNode 0.9
n5 = inputNode 0.99

n6 = SparseNode 0.0 [n1, n2, n3] [1, -1, 3]
n7 = SparseNode 1.0 [n3] [3]
n8 = SparseNode 1.0 [n3, n4, n5] [-1, 1, 0]

n9 = SparseNode 0.0 [n6, n7] [0.1, 1]
n10 = SparseNode 1 [n7, n8] [-1, 1]

n11 = SparseNode 0.0 [n9, n10] [-1, 5]

-- layer
l1 = Layer [n1, n2, n3, n4, n5] 
l2 = Layer [n6, n7, n8]
l3 = Layer [n9, n10]

main = let nodeList = [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11] in
              putStrLn $ show $ fmap evaluate nodeList

module MyLib where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad
import Data.Map.Internal.Debug

data Edge = Edge {
                    edgeDestination :: NodeName,
                    distance :: Int
                } deriving (Show, Eq, Ord)

type NodeName = String

type Nodes = M.Map NodeName Node

data Node = Node {
      nodeName :: NodeName
    , neighbors :: S.Set Edge
    , distanceMap :: DistanceMap
} deriving (Show, Eq, Ord)

type DistanceMap = M.Map (NodeName,NodeName) Distance

--d source destination = min [ c(source,neighbor) + d neighbor destination  | neighbor <- neighbors x]

main :: IO ()
main = do
    let graph = createGraph
    printOutDistanceVector "D" graph
    go graph

go :: Nodes -> IO ()
go graph = 
    let 
        newGraph = cycleDistanceVectorGraphs $ propogateDistanceVectorMaps graph
    in
        if graph == newGraph then
            putStrLn "Done"
        else
            do
                printOutDistanceVector "D" newGraph
                go newGraph

printOutDistanceVector :: NodeName -> Nodes -> IO ()
printOutDistanceVector nodeName nodeMap =
        case M.lookup nodeName nodeMap of
            Just node -> 
                    print 
                    $ (\target -> (target, fromMaybe Infinity $ M.lookup (nodeName, target) (distanceMap node))) <$> (M.keys nodeMap)
            _ -> putStrLn "Node not found"

cycleDistanceVectorGraphs :: Nodes -> Nodes
cycleDistanceVectorGraphs nodes =
    let
        allNodeNames = M.keys nodes
    in
        (\sourceNode -> cycleDistanceVectorGraph sourceNode allNodeNames) <$> nodes


cycleDistanceVectorGraph :: Node -> [NodeName] -> Node
cycleDistanceVectorGraph sourceNode targetNames =
    let
        dMap = distanceMap sourceNode
        sourceName = nodeName sourceNode
        keyValueList = (\targetName -> ((sourceName, targetName), findMinDistanceToTarget sourceNode targetName)) <$> targetNames
    in
        sourceNode { distanceMap= M.fromList keyValueList }


findMinDistanceToTarget :: Node -> NodeName -> Distance
findMinDistanceToTarget node toVertex =
    let
        fromVertex = nodeName node
        neighbors' = S.toList $ neighbors node
        distanceMap' = distanceMap node
        minDistance = minimum $ lookup (fromVertex, toVertex) distanceMap':
                                                 [V costToNeighbor + costFromNeighbor | neighbor <- neighbors'
                                                , let costToNeighbor = distance neighbor --lookup (fromVertex, (edgeDestination neighbor)) distanceMap'
                                                , let costFromNeighbor =  lookup (edgeDestination neighbor, toVertex) distanceMap'
            ]
    in
        minDistance
    where
        lookup (fromVertex,toVertex) map = fromMaybe Infinity $ M.lookup (fromVertex, toVertex) map

propogateDistanceVectorMaps :: Nodes -> Nodes
propogateDistanceVectorMaps nodes =
    (\sourceNode -> 
        let
            neighborNodes = catMaybes $ S.toList $ (\edge -> M.lookup (edgeDestination edge) nodes) `S.map` (neighbors sourceNode)
            newDistanceMaps = (\neighborNode -> mergeDistanceVector sourceNode neighborNode) <$> neighborNodes
        in
            sourceNode { distanceMap = M.unions newDistanceMaps}

    ) `M.map` nodes

mergeDistanceVector :: Node -> Node -> DistanceMap
mergeDistanceVector fromNode toNode =
    let
        sourceMap = filterDistanceMapOnFrom (nodeName fromNode) (distanceMap fromNode)
        targetMap = (distanceMap toNode)
    in
        targetMap `M.union` sourceMap

filterDistanceMapOnFrom :: NodeName -> DistanceMap -> DistanceMap
filterDistanceMapOnFrom nodeName distanceMap = M.filterWithKey (\k _ -> (fst k) == nodeName) distanceMap

data Distance = Infinity | V Int

createGraph :: Nodes
createGraph =
      M.fromList [
        ("A", Node {
            nodeName = "A"
            ,neighbors = S.fromList [
                Edge{edgeDestination="B", distance=14}
            ,   Edge{edgeDestination="C", distance=2}
            ]
            ,distanceMap = M.singleton ("A", "A") (V 0)
      })
      , ("B", Node {
          nodeName = "B"
          ,neighbors = S.fromList [
                Edge{edgeDestination="A", distance=14}  
                ,Edge{edgeDestination="C", distance=8}
                ,Edge{edgeDestination="E", distance=1}
                ,Edge{edgeDestination="F", distance=3}
            ]
            ,distanceMap = M.singleton ("B", "B") (V 0)
      })
      , ("C", Node {
          nodeName = "C"
          ,neighbors = S.fromList [
                Edge{edgeDestination="A",distance=2}
                ,Edge{edgeDestination="B",distance=8}
                ,Edge{edgeDestination="G",distance=1}
            ]
            ,distanceMap = M.singleton ("C", "C") (V 0)
      })
      , ("D", Node {
          nodeName = "D"
          ,neighbors = S.fromList [
                Edge{edgeDestination="E",distance=7}
                ,Edge{edgeDestination="F",distance=10}
            ]
            ,distanceMap = M.singleton ("D", "D") (V 0)
      })
    , ("E", Node {
            nodeName = "E"
            ,neighbors = S.fromList [
                Edge{edgeDestination="B", distance=1}
                ,Edge{edgeDestination="D",distance=7}
                ,Edge{edgeDestination="F",distance=1}
            ]
            ,distanceMap = M.singleton ("E", "E") (V 0)
      })
    , ("F", Node {
          nodeName = "F"
          ,neighbors = S.fromList [
                Edge{edgeDestination="B", distance=3}
                ,Edge{edgeDestination="D",distance=10}
                ,Edge{edgeDestination="E",distance=1}
                ,Edge{edgeDestination="G",distance=3}
                ,Edge{edgeDestination="H",distance=1}
            ]
            ,distanceMap = M.singleton ("F", "F") (V 0)
      })
    , ("G", Node {
            nodeName = "G"
          ,neighbors = S.fromList [
                Edge{edgeDestination="C", distance=1}
                ,Edge{edgeDestination="F",distance=3}
                ,Edge{edgeDestination="H",distance=5}
            ]
            ,distanceMap = M.singleton ("G", "G") (V 0)
      })
    , ("H", Node {
            nodeName = "H"
          ,neighbors = S.fromList [
                Edge{edgeDestination="F", distance=1}
                ,Edge{edgeDestination="G",distance=5}
            ]
            ,distanceMap = M.singleton ("H", "H") (V 0)
      })
    ]

instance Show Distance where
    show (Infinity) = "Infinity"
    show (V v) = show v

instance Num Distance where
    Infinity + _ = Infinity
    _ + Infinity = Infinity
    (V l) + (V r) = V (l + r)

    Infinity * _ = Infinity
    _ * Infinity = Infinity
    (V l) * (V r) = V (l * r)

    abs Infinity = Infinity
    abs (V num) = V (abs num)

    signum Infinity = Infinity
    signum (V num) = V (signum num)

    fromInteger num = V (fromInteger num)

    negate Infinity = Infinity
    negate (V num) = V (negate num)

instance Eq Distance where
    Infinity == Infinity = True
    (V l) == (V r) = l == r
    _ == _ = False

instance Ord Distance where
    compare Infinity Infinity = EQ
    compare Infinity _ = GT
    compare _ Infinity = LT
    compare (V l) (V r) = compare l r
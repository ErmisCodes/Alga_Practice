-- import Algebra.Graph
import Algebra.Graph.Labelled
import Algebra.Graph.Label
-- import Algebra.Graph as UnLabelled
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Hashable
import qualified Data.HashMap.Strict as HMap
import Data.UnionFind.ST
import Control.Monad.ST
import Control.Monad


g1 :: Graph (Distance Int) Int
g1 = edges [(1, 0, 1), (2, 1, 2 ), (3, 2, 3), (4, 3, 4 ), (5, 3, 5)]

g2 :: Graph (Distance Int) Int
g2 = edges [(4,0,1), (11,1,7), (8,0,7), (8,1,2), (7,7,8), (1,7,6), (2,2,8), (6,8,6), (7,2,3),
 (4,2,5), (2,6,5), (14,3,5), (9,3,4), (10,5,4) ]


g3 :: Graph (Distance Int) Int
g3 = edges [(1, 0, 1), (2, 1, 2 ),  (4, 3, 4 ), (5, 3, 5)]


-- Function used to initialize the "union set" of a graph. Returns a hashmap for every vertice to its Point in the union
createUnions :: (Eq a, Hashable a) => [a] -> HMap.HashMap a (Point s a) -> ST s (HMap.HashMap a (Point s a))
createUnions [] hashmap = return hashmap
createUnions (h:t) hashmap = do
    case HMap.member h hashmap of
        True  -> createUnions t hashmap
        False -> do
            newPoint <- fresh h
            let hashmap' = HMap.insert h newPoint hashmap
            createUnions t hashmap'

-- Takes the hashamp from vertices to UnionPoints, and the edges of the graph sorted and applies Kruskal's algorithm
kruskalBase :: (Hashable a, Eq a, Ord a, Ord e, Num e) =>  HMap.HashMap a (Point s a) -> [(Distance e, a, a)] -> Int -> Int -> [(Distance e, a, a)] -> ST s (Maybe [(Distance e, a, a)])
kruskalBase pointMap [] used target result = do
    case used == target of
        True  -> return $ Just result
        False -> return Nothing
kruskalBase pointMap ((d,u,v):t) used target result = do
        let pointU = fromJust $ HMap.lookup u pointMap       -- fromJust won't throw an exception if used from inside the Kruskal function
        let pointV = fromJust $ HMap.lookup v pointMap
        reprU <- repr pointU
        reprV <- repr pointV
        case (reprU == reprV) of True  -> kruskalBase pointMap t used target result
                                 False -> do
                                    Data.UnionFind.ST.union (reprU) (reprV)
                                    kruskalBase pointMap t (used + 1) target ((d,u,v):result)


-- Takes as input an Alga Labelled Graph and applies Kruskal's algorithm to it, using the 2 helper functions above
kruskal :: (Hashable a, Eq a, Ord a, Ord e, Num e) => Graph (Distance e) a -> ST s (Maybe [(Distance e, a, a)])
kruskal graph = do
    starting_union <- createUnions (vertexList graph) HMap.empty
    let edgeListSorted =  sortOn (\(a,b,c)-> a ) $ edgeList graph
    let target = (length $ vertexList graph) - 1
    kruskalBase starting_union edgeListSorted 0 target []

-- to run the algorithm: runST $ kruskal <Labelled Graph>

-- import Test.QuickCheck
import Algebra.Graph
import Control.Applicative

myGraph :: Graph Int
myGraph = (Vertex 5) + Empty


myGraph2 :: Graph Int
myGraph2 = 1 * ( 2 + 3)


myGraph3 :: Graph Int
myGraph3 = Empty + (Vertex 5)

myfun :: Int -> Int
myfun x = x + 1


close :: Graph a -> Graph a               -- PATHOGENIC FUNCTION
close Empty         = Empty
close (Vertex x)    = Vertex x
close (Overlay x y) = Connect x y
close (Connect x y) = Connect x y


getOutNeighbors :: (Eq a, Ord a)  => Graph a -> a -> [a]
getOutNeighbors g node = map snd . filter (\(v,_) -> v == node ) $ edgeList g


getInNeighbors :: (Eq a, Ord a)  => Graph a -> a -> [a]
getInNeighbors g node = map fst . filter (\(_,v) -> v == node ) $ edgeList g


-- isConnected :: Int -> Bool



-- numberOfConnected ::  Graph Int -> IO Int
-- numberOfConnected = fmap (length . filter id) .
-- 	foldg (pure Empty) (fmap Vertex . isConnected) (liftA2 Overlay) (liftA2 Connect) .
--   	vertexList

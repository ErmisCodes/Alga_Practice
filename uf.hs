import Data.UnionFind.ST
import Control.Monad.ST
import Control.Monad


foo :: a -> ST s (Point s a)
foo bar = fresh bar

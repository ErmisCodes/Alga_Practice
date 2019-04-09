{-# LANGUAGE DeriveGeneric #-}   -- in case we need predifined Coarbitraries for our trees

import Control.Monad
import  Test.QuickCheck
import Data.Ratio
import Text.Show.Functions   -- This makes all functions derive show, necessary for them to be tested with quickCheck
import GHC.Generics          -- I think we just reached a new low point...


data Tree a = T a [Tree a]
    deriving (Show,Generic)


foldTree :: (a -> [b] -> b) -> Tree a -> b   -- Folds a tree in the same way foldr folds a list
foldTree f (T x ts) = f x bs where
    bs = map (foldTree f) ts


add_nums ::  Num a => a -> [a] -> a  -- calculates the sum of an integer and a list of integers
add_nums x [] = x
add_nums x list = foldl (+) x list

sumTree ::  Num a => Tree a -> a   -- calculates the sum of all the nodes of an Integer tree
sumTree t = foldTree add_nums t

-- The first arguement of size is needed just for type consistency
size :: Num b =>  a -> [b] -> b  -- Given a node and the size of its children in a list, it calculates their size
size _ [] = 1 -- If we are at a leaf, the size of the leaf is 1
size _ children = foldl (+) 1 children  -- The size of the node is equal to 1+ the size of its children

sizeTree:: Num b => Tree a -> b
sizeTree t = foldTree size t

 -- The first arguement of height is needed just for type consistency
height :: (Ord b, Num b)  => a -> [b] -> b  --takes the height of all the children of the current node and calculates the node's height
height _ [] = 1 -- Every leaf has a height of 1
height _ list = (find_max list 1) + 1 where  -- The height of the current node is 1 + the height of its tallest child
    find_max [] curr = curr
    find_max (x:xs) curr =
        if (x > curr) then find_max xs x else find_max xs curr


heightTree:: (Ord b, Num b)  => Tree a -> b
heightTree t = foldTree height t


node_max :: Ord a => a -> [a] -> a
node_max x [] = x -- The max of a leaf is its value
node_max x list = find_max list x where  -- The max value of a node is either its own value or the max of its children
    find_max [] curr = curr
    find_max (x:xs) curr =
        if (x> curr) then find_max xs x else find_max xs curr


maxTree :: Ord a => Tree a -> a
maxTree t = foldTree node_max t



add_contents:: a -> [[a]] -> [a]  --
add_contents curr [] = [curr]  -- If the node is a leaf, all its contents are its own value
add_contents curr list = curr:(concat list)  --Else, its contents are the values of all its childres + its own


nodes :: Tree a -> [a]   -- This implementation shows the value of all the nodes, each value
nodes t = foldTree add_contents t -- shown as many times as it apperas on the tree


inTree :: Eq a => a -> Tree a -> Bool
inTree x t = is_member x tree_nodes where   -- The most "logical" way of defining inTree is through the nodes function
    tree_nodes = nodes t   -- there are however faster implementations, because this needs to traverse the whole tree twice
    is_member _ [] = False
    is_member target (t:ts) = if (target == t) then True else is_member target ts


count:: (a-> Bool) -> a -> [Integer] -> Integer
count f a [] = if (f a) then 1 else 0   -- Check if a leaf has the property in question
count f a list = if (f a) then foldl (+) 1 list else foldl (+) 0 list -- The count up to the node we are currently at
-- should be 1/0 + the count of its children, depending on if the property was true/false for the node


countTree:: (a -> Bool) -> Tree a -> Integer
countTree f t = foldTree (count f) t

-- leaves_only :: a -> [a] -> [a]
-- leaves_only a [] = [a]  -- If the current node is a leaf, the result should be its value
-- leaves_only a list = list  -- If the current node isn't a leaf, we only keep the result of its children
--
--
-- leaves:: Tree a -> [a]
-- leaves t = foldTree leaves_only t  -- A reminder that concat is useful...

only_leaves:: a -> [[a]] -> [a]
only_leaves curr [] = [curr]   -- If the current node is a leaf, returns a list with its value
only_leaves _ list = concat list  -- If it isn't, take the list of lists of all its children, and return its concatination

leaves:: Tree a -> [a]
leaves t = foldTree only_leaves t



mapper:: (a -> b) -> a -> [Tree b] ->  Tree b
mapper f a [] = T (f a) []  -- If we are at a leaf, we create a tree with value f a and no children
mapper f a children = T (f a) children -- if we are not a leaf, we map ourselves, and adopt the children below us

mapTree:: (a -> b) -> Tree a -> Tree b
mapTree f t = foldTree (mapper f) t  -- Works properly, but cannot display its result because type tree doesn't derive Show


-------------------------- PART 2

trimTree:: Int -> Tree a -> Tree a
trimTree 0 (T x ts) = T x []     -- If trim has reached zero, we do not trim the tree any further
trimTree n (T x ts) = T x ts' where   -- Else, we keep ourselves and trim our children
  ts' = map (trimTree (n-1)) ts

path:: [Int] -> Tree a -> a
path [] (T x ts) = x   -- We have reached our destination
path (l:ls) (T x ts) = path ls child where
  child = choose l ts    -- Child is the Nth element of the children list
  choose 0 (t:ts) = t
  choose n (t:ts) = choose (n-1) ts




t = T 1 [ T 2 []
          , T 3 [ T 4 []
                  , T 5 [ T 7 [], T 8 [], T 9 []]
                  ]
          , T 6 [ T 10 [] , T 11 [], T 12 [ T 13 [] , T 14 [T 15 []]]]
          ]   -- a sample Integer tree to test things


-- instance Arbitrary a => Arbitrary (Tree a)

-- instance Arbitrary a => Arbitrary (Tree a) where
-- arbitrary = sized arbTree

-- FIRST TREE IMPLEMENTATION

{-
instance Arbitrary  a => Arbitrary (Tree a) where   -- To create an Arbitrary instance for a datatype, we must declare its arbitrary function
  arbitrary = sized arbTree where     -- The types make sense : The result of arbitrary must be of monad Gen
    arbTree:: Arbitrary a =>  Int -> Gen ( Tree a)  --Takes our size (Int) and returns a corresponding tree
    arbTree 0 = do    -- if size = 0, we are a leaf
      a <- arbitrary
      return $ T a []
    arbTree n = do     -- if size = n, then we have close to n grand children (non-Leaves)
      (Positive m) <- arbitrary   -- Randomly choose how many children our current node will have (m)
      let n' = n `div` (m+1)  --Each of our children will have m children (empty or not) , but ~n' grandchildren
      a <- arbitrary
      f <- replicateM m (arbTree n')    -- this causes the Tree to be a little bit "too balanced"
      return $ T a f

This implementation is basically 100% stolen from Stack Overflow
but it is very elegant

Similar implimentation to the one above, with different children distribution

instance Arbitrary  a => Arbitrary (Tree a) where   -- To create an Arbitrary instance for a datatype, we must declare its arbitrary function
  arbitrary = sized arbTree where     -- The types make sense : The result of arbitrary must be of monad Gen
    fromGen:: Gen a -> a   -- a function that takes an argument of type Gen a and returns its value a
    arbTree:: Arbitrary a =>  Int -> Gen ( Tree a)  --Takes our size (Int) and returns a corresponding tree
    arbTree 0 = do    -- if size = 0, we are a leaf
      a <- arbitrary
      return $ T a []
    arbTree n = do     -- if size = n, then we have close to n grand children (non-Leaves)
      (Positive m) <- arbitrary   -- Randomly choose how many children our current node will have (m)
      let n' = choose (0, n )  --Each of our children will have m children (empty or not) , but ~n' grandchildren
      a <- arbitrary
      f <- replicateM m (arbTree n')    -- this causes the Tree to be a little bit "too balanced"
      return $ T a f


THE ONE ABOVE IS A MODIFICATION OF THE FIRST ONE

-}


instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbitrarySizedTree where
        arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
        arbitrarySizedTree m = do
            t <- arbitrary
            n <- choose (0, m `div` 2)  -- we have n kids, n is between 0 and m/2
            new_n <- choose (0, (2*m) `div` 3 )
            ts <- vectorOf n (arbitrarySizedTree new_n)   --each of our children has an m between 0 and 2/3, so an average of 1/3 of ours
            return (T t ts)  -- this creates a fairly "balanced" looking, random tree

-- A more original, albeit uglier implementation, but generates "randomly balanced" trees


prop_heigth:: Tree a -> Bool  --checks wether the height of a tree is less than its size (and a valid number).
prop_heigth t = ( (heightTree t) <=  (sizeTree t)) && ((heightTree t ) >= 0 ) && ((div (heightTree t) 1) ==  heightTree t)
-- If the height is not a natural number, the last check will raise an error
-- If we want to check with a specific type, all we need to do is call quickCheck with that type

prop_max_in_tree :: Ord a => Tree a -> Bool  -- Checks whether the max value of a tree belongs to the nodes of that tree
-- prop_max_in_tree ::  Tree Int -> Bool
prop_max_in_tree t = inTree (maxTree t) t
-- If we want to test for a specific type of tree, we just need to specify that on QuickCheck in the form : quickCheck (prop_max_in_tree:: Tree <TYPE> -> Bool )

prop_all_nodes_in_tree :: Eq a => Tree a -> Bool  -- Checks that the values of all nodes in a tree belong to that tree
-- prop_all_nodes_in_tree :: Tree Int -> Bool
prop_all_nodes_in_tree t = exist node_list t where
    node_list = nodes t
    exist:: Eq a => [a] -> Tree a -> Bool
    exist [] _ = True
    exist (x:xs) t =
        if (inTree x t) then exist xs t
        else False
-- Alternatively, we can call it in the form quickCheck (prop_all_nodes_in_tree :: Tree <OUR TYPE> -> Bool)


prop_count_less_than_size:: (a -> Bool) -> Tree a -> Bool  -- Checks whether the number of nodes of that tree that satisfy f is less than the size of the tree
prop_count_less_than_size f t = elements <= tree_size where
    elements = countTree f t
    tree_size = sizeTree t

prop_size_properties :: Tree a -> Bool  -- Checks the one with the many size properties...
prop_size_properties t = (length (nodes t) == sizeTree t) &&  ( ( (length (leaves t)) <  ( length (nodes t) )  ) || ( (length (leaves t)) == 1 && (length (nodes t)) == 1) )
-- Number of nodes == size of the tree     &&   the leaves part

prop_map_ratains_shape :: (a -> b ) -> Tree a -> Bool  -- Checks whether the mapped tree has the same size and height as the initial one
prop_map_ratains_shape f t = (sizeTree t == sizeTree t' ) && (heightTree t == heightTree t') where
    t' = mapTree f t

prop_map_in_tree:: Eq b => (a -> b) -> Tree a -> Bool   -- Checkes that for every element n in the tree, f n exists IN THE SAME PLACE in the mapped tree
prop_map_in_tree  f t = nodes_A == nodes_B where
    nodes_A = map f (nodes  t)    --nodes t has all elements of the tree, so nodes_A by definition has every single f n of the tree
    nodes_B = nodes (mapTree f t)   -- nodes b has all the nodes of the mapped tree
    -- types = f :: Eq b => a -> b


prop_fog_and_gof :: ( Int -> Int ) ->  (  Int -> Int ) -> Tree Int  -> Bool  -- WORKS CORRECTLY, BUT FAILS BECAUSE THE PROPERTY SHOULD FAIL,
prop_fog_and_gof f g t =  ( map (f . g_nodes)  $  [t] ) !! 0   ==   ( ( g_nodes . ( mapTree f ) ) t ) where -- FOR most functions f .g != g. f
    g_nodes tree  =  g $ ( nodes tree ) !! 0  -- hoever, if we run it with given, linear functions, it works correctly




-- fog and gof, in the way they were declared in the exercise, had some type issues.
-- this was a very very ugly way of working around them :
-- to work around coarbitraries, we ask generate to create functions from the type of the tree, to that type
-- and then with the g_nodes part, we "wrap" them in a function that takes the arguement of the first node of the tree
-- and applies the generated function to its value. Because of the way we generate trees, chosing the first node vs any other
-- makes no distribution difference.
-- however, the property still fails, because it's simply not correct.
-- Counterexample : f x = 5 * x   and  g x = x + 1. Then, fog != gof

--      --RELEVANT BINDINGS --
-- g_nodes :: Tree Int -> Int (But because it takes the HEAD of the tree, it's actually from Node of Tree -> Int)
-- f.g_nodes :: Tree Int -> Int
--  map (f . g_nodes ) :: [Tree Int ] -> [Int]
--  map (f . g_nodes )   !! 0 :: [Tree Int] -> Int

-- RIGHT PART OF EQUALS :
-- mapTree f :: Tree Int -> Tree Int
-- g_nodes . mapTree f :: Tree Int -> Int



-- We misunederstood the task. This is what we SHOULD have done:
prop_nodes_and_leaves :: Eq b => (a -> b) -> Tree a ->   Bool -- the function f is, in the general case, can be polymorphic
prop_nodes_and_leaves f t =  ( ( ( map  f ). nodes )  t ) == ( (nodes . (mapTree f ) ) t )
-- for every function f we generate, we test it against both possible instances of g



-- These were just some debugging properties :

-- prop_is_same :: Tree Int -> Bool
-- prop_is_same t = nodes ( mapTree (+ 2) t ) == nodes ( mapTree (+ 1) $ mapTree (+ 1) t )
--
-- prop_is_same_again :: ( Int -> Int ) -> Tree Int ->  Bool
-- prop_is_same_again f t = ( f $ ((nodes t) !! 0 ) )  == ( f $ ( nodes ([t]  !! 0 ) !! 0 ) )
--
-- prop_is_same_2 :: ( Int -> Int ) -> Tree Int -> Bool
-- prop_is_same_2 f t = ( f_nodes t ) == (f_nodes t) where
--     f_nodes tree  =  f $ ( nodes tree ) !! 0



--All the properties are polymorphic: To test them with a specific type of tree, we must run :
-- <our Checker> ( <desired_property> :: <desired tree type> -> Property | Bool )

bird :: Tree Rational
bird = T (1 % 1) [left, right] where
    left = mapTree (\x -> (denominator x)  % ( (numerator x) + (denominator x) ) ) bird   --Are we one line above the 3 line challenge, or does
    right = mapTree (\x -> ( (numerator x) + (denominator x) ) % (numerator x )   )   bird  --  the type  declaration not count?



prop_bird_path :: [Int] -> Bool  -- Returns whether following the same path of length n on the bird tree and its trimmed version has the same result
prop_bird_path list = path list' bird  == path list' (trimTree n bird )  where
    list' = map ( \x -> mod (abs x) 2  ) list  -- This creates a list of 0s and 1s with the same probability,
    n = length list  -- so that we take a relatively random path in the tree


prop_zig_zag :: Integer -> Bool -- Checks if the zig-zag property holds true for the first n steps
prop_zig_zag n = path_list (take (fromInteger $ abs n) $ cycle [1,0] ) [] bird == zipWith (%) [1..(abs n)] (take (fromInteger $ abs n) $ cycle [1]) where -- take n $ cycle [1,0] creates a list of length n and alternating ones and zeros
    path_list [] acc _ = reverse acc
    path_list (0:xs) acc (T a [left, right]) = path_list xs (a:acc) left  -- This solution has O(n) complexity, but if we used the
    path_list (1:xs) acc (T a [left, right]) = path_list xs (a:acc) right -- Previously defined path, we would be at O( N^2)

fibs = 1:1: zipWith (+) (fibs) (tail fibs)   -- We create the classic fibonacci list, with the added twist that we discard the first zero

prop_fibs_path :: Int -> Bool  -- Returns whether following the leftmost path gives us th
prop_fibs_path n = new_path_list (take  (abs n) $ repeat 0 ) [] bird == take (abs n) (tail fibs) where  -- The fibonacci part of the tree "skips" the first 0:1 and then starts
    new_path_list [] acc _ = reverse acc
    new_path_list (0:xs) acc (T a [left, right]) = new_path_list xs ( (denominator a):acc) left
    new_path_list (1:xs) acc (T a [left, right]) = new_path_list xs ((denominator a):acc) right -- The same function as before, but now it keeps only the denominator


prop_rational_in_tree ::  Rational -> Bool
prop_rational_in_tree n =  n == path rational_path bird where
    rational_path = path_list bird 0  n []
    path_list :: Tree Rational -> Integer -> Rational -> [Int] -> [Int] -- Takes the TREE -> FLAG -> Target -> accumulator -> accumulator
    path_list (T a [left, right]) flag target acc =    -- This function finds the path from the start of the tree to a rational number
        if (a == target) then reverse acc  -- The thought process is simple: In each node of the Bird Tree, all nodes to its left are smaller than the
        else if ( (target > a) && flag == 0   ) then path_list right 1 target (1:acc) --current node and all the nodes to its right are greater
        else if ( flag == 0  ) then path_list left 1 target (0:acc)  -- if we have taken an even number of steps and vice versa
        else if ( (target > a) && flag == 1  ) then path_list left 0 target (0:acc)
        else path_list right 0 target (1:acc)

sizedRational ::  Gen Rational
sizedRational = sized $ \n -> do
    denom <- choose ( 1, n + 1)  -- So that the denominator is always greater than zero
    num <- choose (1 ,  n + 1)    -- So that the numerator has the same average as the denominator (and is positive)
    return ((toInteger num) % (toInteger denom ) )

-- Can be called in the form : generate (resize <THE SIZE WE WANT> sizedRational to create a rational with that parameter)

-- To Control the size of the result, in the property  rational in tree we can use:
--  verboseCheck (forAll (resize <The Size We Want> sizedRational) prop_rational_in_tree)

-- OR , we can simply call :

prop_rational_in_tree_sized :: Property  -- Yes, it's type is a property, isn't that awesome???
prop_rational_in_tree_sized = forAll (resize 1000000 sizedRational) prop_rational_in_tree



-- Using the one above is the same as :

prop_rational_in_tree_unsized ::  Rational -> Bool
prop_rational_in_tree_unsized n = my_abs n == path rational_path bird where
    my_abs :: Rational -> Rational    -- by trimming the tree at the point numerator n + denominator n, we can be sure that we have taken "enough" steps to incude n
    my_abs n = if ( numerator n == 0 ) then abs ( 1 % denominator n)  -- but also make sure that inTree will not construct an infinite list of contents
        else abs n    -- We need this function to make sure we get rational numbers  greater than zero
    rational_path = path_list bird 0  (my_abs n) []
    path_list :: Tree Rational -> Integer -> Rational -> [Int] -> [Int] -- Takes the TREE -> FLAG -> Target -> accumulator -> accumulator
    path_list (T a [left, right]) flag target acc =    -- This function finds the path from the start of the tree to a rational number
        if (a == target) then reverse acc  -- The thought process is simple: In each node of the Bird Tree, all nodes to its left are smaller than the
        else if ( (target > a) && flag == 0   ) then path_list right 1 target (1:acc) --current node and all the nodes to its right are greater
        else if ( flag == 0  ) then path_list left 1 target (0:acc)  -- if we have taken an even number of steps and vice versa
        else if ( (target > a) && flag == 1  ) then path_list left 0 target (0:acc)
        else path_list right 0 target (1:acc)

-- The difference is that now the size can grow so big that the programm will take forever to finish


main = do
    putStrLn "First we check all the ``general`` properties, with an Int Tree :"
    putStrLn "Checking that height is a nutural number, less than the size of the tree:"
    quickCheck (prop_heigth :: Tree Int -> Bool)
    putStrLn "Checking that the max of a tree belongs to that tree:"
    quickCheck (prop_max_in_tree :: Tree Int -> Bool)
    putStrLn "Checking that all nodes of a tree belong to that tree:"
    quickCheck (prop_all_nodes_in_tree :: Tree Int -> Bool)
    putStrLn "Checking that the number of nodes that satisfy a property is <=  the size of a tree:"
    quickCheck (prop_count_less_than_size  :: (Int -> Bool) ->  Tree Int -> Bool)  -- We leave the propert to be generated randomly
    putStrLn "Checking that property that does a lot of things with size..."
    quickCheck (prop_size_properties :: Tree Int -> Bool )
    putStrLn "Checking that maping a tree retains its size and shape:"
    quickCheck (prop_map_ratains_shape :: (Int -> Int) -> Tree Int -> Bool)  -- We map the tree with a ranomly generated function from Int -> Int
    putStrLn "Checking that for every value that belongs to the nodes of a tree, f n belongs to the mapped tree, where f random:"
    quickCheck (prop_map_in_tree:: (Int -> Int) -> Tree Int -> Bool)
    putStrLn "Checking that for every g, map f.g  == g. mapTree f:"
    quickCheck (prop_fog_and_gof)
    putStrLn "It failed, because it was supposed to fail. See comments on lines 234-240"
    putStrLn "It wasn't really what the exercise wanted, but it was too fun not to include!"
    putStrLn "Checking the last property of ex3.pt2, about mappings:"
    quickCheck (prop_nodes_and_leaves:: (Int ->Int) -> Tree Int -> Bool)
--  HAVENT FIXED THIS ONE YET
    putStrLn " ---- Now we are Checking the bird tree properties  ----"
    putStrLn "Checking that a path leads to the same node on the bird tree and its trimmed cousin:"
    quickCheck prop_bird_path
    putStrLn " Checking that following a zig-zag path will lead us to all natural numbers in succession"
    quickCheck prop_zig_zag
    putStrLn "Checking that the left most path has as denominators successive Fibonacci numbers:"
    quickCheck prop_fibs_path
    putStrLn "Checking that we can find any rational number we want in the bird tree:"
    quickCheck prop_rational_in_tree_sized  -- We have created a sized generator so that things do not get TOO out of hand

module Data.Trees.RedBlackTreeV2 where
import qualified Data.List as L

data Color  = R | B deriving (Eq, Show)
data Tree a = Empty | Node Color !(Tree a) a !(Tree a) deriving (Eq, Show)

{--
 Inserts the given ordred element into the given Tree and takes care of rebalacing it.
 Rebalancing is only needed when a black parent node is encountred otherwise no rebalancing is done.
 Rebalancing is done just for B-R-R cases.

 Note that this function forces the root's color to Black (let ... in Node B l a r)
 we need the root to be Black because during rebalancing, this node is not taking into acount,
 because it's parent is NULL (root), thus rebalancing of this node never occurs(we have to force it explicitly).
 Note also that newly created nodes are Red
--}
insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Node R Empty x Empty
insert t x =
   let insert' Empty = insert Empty x
       insert' s@(Node B l v r)
        | (x == v)  = s
        | (x <  v)  = rebalanceL (insert' l) v r
        | otherwise = rebalanceR l v (insert' r)

       insert' s@(Node R l v r)
        | (x == v)  = s
        | (x <  v)  = Node R (insert' l) v r
        | otherwise = Node R l v (insert' r)

       Node _ l a r = insert' t
   in Node B l a r


{--
 2 cases B-R-R : in all cases after rotation we set (root-color = R, {left,right}-color = B)
   - (root black, left red and left left red)=>Left-rotation around root
   - (root black, left red and left right red)=> Left-rotation around left and the right rotation around root

-Note that we don't need to test the color of the given Tree in the pattern, when defining rebalanceL (the same thing holds for rebalanceR)
because insert' takes care of testing it before calling this function, thus we just need to match RED-RED nodes.
insert' calls this function just whe it match a black node.

-Note also that the 3th case just reconstruct the origin Tree (Node B a x b) because
insert' calls this function when it encounters a Black node without passing the color. We know the color of this node from
the calling context (insert')
--}
rebalanceL :: Tree a -> a -> Tree a -> Tree a
rebalanceL (Node R (Node R l v3 r'') v2 r') v1 r = Node R (Node B l v3 r'') v2 (Node B r' v1 r)
rebalanceL (Node R l v2 (Node R l' v3 r')) v1 r  = Node R (Node B l v2 l') v3 (Node B r' v1 r)
rebalanceL a x b = Node B a x b

{--
 2 cases B-R-R : in all cases after rotation we set (root-color = R, {left,right}-color = B)x
   - (root black, right red and right's right red) => Right-rotation around root
   - (root black, right red and right's left red ) => RIght-rotation around right and then left-rotation around root

Note that we don't need to test the color of the given Tree in the pattern, when defining rebalanceR (the same thing holds for rebalanceL)
because insert' takes care of testing it before calling this function, thus we just need to match RED-RED nodes.
insert' calls this function just whe it match a black node

Note also that the 3th case just reconstruct the origin Tree (Node B a x b) because
insert' calls this function when it encounters a Black node without passing the color. We know the color of this node from
the calling context (insert')
--}
rebalanceR :: Tree a -> a -> Tree a -> Tree a
rebalanceR  l v1 (Node R l' v2 (Node R l'' v3 r)) = Node R (Node B l v1 l') v2 (Node B l'' v3 r)
rebalanceR  l v1 (Node R (Node R l' v3 r') v2 r)  = Node R (Node B l v1 l') v3 (Node B r' v2 r)
rebalanceR  a x b = Node B a x b

{--
    Function that returns whether the given Tree contains the given ordred element or Not.
--}
contains :: Ord a => Tree a -> a -> Bool
contains Empty x = False
contains (Node _ l v r) x | x < v = contains l x
                          | x > v = contains r x
                          |otherwise = True

-- in-order depth-first traversal of the tree (the list result is in ascending order)
toSortedList :: Tree a -> [a]
toSortedList Empty = []
toSortedList (Node _ l v r) = toSortedList l ++ (v:toSortedList r)

-- pre-order depth-first traversal of the tree
toList :: Tree a -> [a]
toList Empty = []
toList (Node _ l v r) = v:toList l ++ toList r

-- in-order depth-first traversal right first and then left (produces an ordered descending list)
toSortedList' Empty = []
toSortedList' (Node _ l v r) = toSortedList' r ++ v:toSortedList' l

{--
   Retruns how many Reds and Blacks in the given Tree as (redcount, blackcount)
--}
countRB ::(Num b, Num c) => Tree a -> (b, c)
countRB Empty = (0,0)
countRB (Node c l _ r) = let (r1,b1) = countRB l
                             (r2,b2) = countRB r
                             (rr,br)   = if c == R then (1,0) else (0,1)
                         in  (rr+r1+r2, br+b1+b2)

{--
   Creates a new Red-Black Tree from a given list
--}
fromList :: Ord a => [a] -> Tree a
fromList = L.foldl' insert Empty

{--
 Note that we can check that all paths from root to leaves are equals by
 grouping all lengths of paths and having the length of the resulting list <= 1
 instead of : all (== head xs) use : lenght (group xs) <= 1.

 Red Black Tree =
 1) No Red node has Red chidren (Red-Red problem)
 2) All paths from root to leaves contain same Black node count
--}
isRBTree :: Eq a => Tree a -> Bool
isRBTree Empty = True
isRBTree t = and [equalBlackCount, noRedRed t]
  where  equalBlackCount = all (== head bCount) $ tail bCount
         bCount = map (length . filter (== B) . map fst) $ paths t


{--
  Returns whether the given tree contains Red-Red nodes or not
--}
noRedRed :: Tree a -> Bool
noRedRed Empty = True
noRedRed (Node c l _ r) = not ((c == R) && ((color l == R)||( color r == R)))
                          && noRedRed l && noRedRed r

{--
  Returns the color of the given Node.
Note that the Empty node is always Black
--}
color :: Tree a -> Color
color Empty = B
color (Node c _ _ _) = c

{--
  Returns all paths from root to leaves in pre-order depth first traversal
--}
paths :: Tree a -> [[(Color, a)]]
paths Empty = []
paths (Node c Empty v Empty) = [[(c,v)]]
paths (Node c l v r) = map ((c,v):) (paths l ++ paths r)

---------------------------------------------------------------------------------
-- Test section
---------------------------------------------------------------------------------

t1 = fromList [1..7]
t2 = insert t1 8
t3 = insert t2 9
testContains = map (contains t1)[1..20]
badBlackCount = Node B (Node B Empty 2 Empty) 5 (Node R Empty 7 Empty)
badColor = Node B (Node B Empty 2 Empty) 5 (Node R (Node R Empty 8 Empty) 10 Empty)

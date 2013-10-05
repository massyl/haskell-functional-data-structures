module Data.Trees.AVL where
import qualified Data.List as L

{-- Note that to be more effecient we have store the high of each subtree in the node to avoid recalculating it--}
data Tree a = Empty | Node a !(Tree a) !(Tree a) deriving (Eq,Show)

leaf x = Node x Empty Empty

-- Some examples of structure in code
t1 = Node 10 (leaf 8) (leaf 15)
t2 = Node 17 (Node 12 (Node 5 (leaf 4) (leaf 8)) (leaf 15))
             (Node 115
                     (Node 32 (leaf 30) (Node 46 (leaf 43) (leaf 57)))
                              (Node 163 (leaf 161) Empty))

size :: Num a => Tree b -> a
size Empty = 0
size (Node _ l r) = 1 + size l + size r

height Empty = -1
height (Node _ Empty Empty) = 0
height (Node a l r) = 1 + max (height l) (height r)

-- Return a unsorted list, but we can go back to the origin Tree from this list
toList :: Tree a -> [a]
toList Empty = []
toList (Node a l r) = a:toList l ++ toList r

-- Returns a sorted list of all elements of the given Tree. Note that we can't go back to the origin Tree
toSortedList :: Tree a -> [a]
toSortedList Empty = []
toSortedList (Node a l r) = toSortedList l ++ a:toSortedList r

smallValue :: Tree a -> Maybe a
smallValue Empty = Nothing
smallValue (Node a Empty _) = Just a
smallValue (Node _ l _) = smallValue l

greatValue :: Tree a -> Maybe a
greatValue Empty = Nothing
greatValue (Node a _ Empty) = Just a
greatValue (Node _ _ r) = greatValue r

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node a l r) = Node a (mirror r) (mirror l)

empty :: Tree a -> Bool
empty Empty = True
empty _ = False

contains :: Ord a =>  Tree a -> a -> Bool
contains Empty _ = False
contains (Node a l r) x | x == a = True
                        | x < a = contains l x
                        | x > a = contains r x

rightSon :: Tree a -> Maybe (Tree a)
rightSon Empty = Nothing
rightSon (Node _ _ r) = Just r

leftSon :: Tree a -> Maybe (Tree a)
leftSon Empty = Nothing
leftSon (Node _ l _) = Just l

{--
  Insert an new ordred value into the tree.
  Note that it preserves the Binary Search tree propertie,
  and the H-balanced propertie of an AVL.
--}
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty e = leaf e
insert t@(Node a l r) e | (e == a)  = t
                        | (e < a )  = rebalance (Node a (insert l e) r)
                        | otherwise = rebalance (Node a l (insert r e))
{--
 Removes an element from the tree: suppose we want to remove x from t
 - Empty t = nothing to do
 - x < (t's root element) = recursive call to remove x in left subtree and then rebalance the tree
 - x > (t's root element) = recursive call to remove x in right subtree and then rebalance the tree
 - x == (t's root element) in the following cases :
   - Empty (t's leftson) = return rightson
   - Empty (t's rightson)= return leftson
   - not Empty left and right sons of t = replace (t's element) by the maximum element in left subtree
     (or minimum element in right subtree) and rebalance
--}

remove :: (Ord a) => Tree a -> a -> Tree a
remove Empty _ = Empty
remove (Node a l r) x | a > x = rebalance $ Node a (remove l x) r
                      | a < x = rebalance $ Node a l (remove r x)
remove (Node a Empty r) x = r
remove (Node a l Empty) x = l
remove (Node a l r) x = rebalance (Node v vs r) where (v,vs) = deleteMax l
{--
 Deletes the maximum element in a given Tree.
 Note that this implementation works only on non Empty Trees.
 Note that we can handle easly this success or failure of this function by returning Maybe (a, Tree a) or Either(String, (a, Tree a))
--}
deleteMax :: Tree a -> (a, Tree a)
deleteMax (Node a l Empty) = (a, l)
deleteMax (Node a l r) = (v, Node a l vs) where (v,vs) = deleteMax r

{--
 returns whether the given tree is a binary search tree or not
--}
isBSearchTree :: (Ord a) => Tree a -> Bool
isBSearchTree Empty = True
isBSearchTree (Node _ Empty Empty) = True
isBSearchTree (Node a l@(Node b _ _) Empty) = (a > b) && (isBSearchTree l)
isBSearchTree (Node a Empty r@(Node b _ _)) = (a < b) && (isBSearchTree r)
isBSearchTree (Node a l1@(Node b _ _) l2@(Node c _ _)) = and [(a > b), (a < c), (isBSearchTree l1), (isBSearchTree l2)]

{--
 Right rotate the given Tree. suppose we have tree N, NL(NLL,NLR),NR(NRL,NRR),
 - Empty = do nothing
 - N NL R = NL become the root with NLL as it's left son and the tree rooted with N(NLL,NR) as it's right son
   (note that the right son of NL becomes the left son of N after the rotation and N because the right son of NL
--}
rotateR :: Tree a -> Tree a
rotateR Empty = Empty
rotateR (Node a (Node b l' r') r) = Node b l' (Node a r' r)

{--
 Left rotate the given Tree. suppose we have tree N, NL(NLL,NLR),NR(NRL,NRR),
 - Empty = do nothing
 - N NL R = NR become the root with N (NL,NRL) as it's left son and NRR as it's right son
   (note that the left son of NR becomes the right son of N after the rotation and N because the left son of NR
--}
rotateL :: Tree a -> Tree a
rotateL Empty = Empty
rotateL (Node a l (Node b l' r')) = Node b (Node a l l') r'

{--
 Function that rebalance the Tree after insertion or deletion of values.
 4 Cases to considere : suppose that the
    - current Node is N
    - NL = left son, NLL= left left son, NLR= left right son
    - NR= right son, NRL = right left son, NRR= right right son

 -Empty tree = already balanced
 -Empty NL = if already balanced nothing to do otherwise do one left rotation on N
 -Empty NR = if already balanced nothing to do otherwise do one right rotation on N
 -NL and NR not Empty = if already balanced nothing to do otherwise :
   1)- if (hight NL) - (hight NR) == 2
     1-1) if (hight NLL) - (hight NLR) >=0 do right rotation on N
     1-2)otherwise do a left rotation on NL and then a right rotation on N

   2)- if (hight NL) - (hight NR) == -2
     2-1) if( hight RRL) - (hight RRL) >= 0 do a left rotation on N
     2-1) otherwise do a right rotation on NR and then a left rotation on N
--}
rebalance Empty = Empty
rebalance t@(Node a Empty r@(Node b l' r'))| (balanced $ factor r Empty) = t
                                           | otherwise  = rotateL t

rebalance t@(Node a l@(Node b l' r') Empty)| (balanced $ factor l Empty) = t
                                           | otherwise  =  rotateR t

rebalance t@(Node a l@(Node b l' r') r@(Node c l'' r''))
  |balanced $ factor l r = t
  |(factor l r == 2 )    = if (factor l' r'  ) >= 0 then rotateR t else rotateR $ Node a (rotateL l) r
  |(factor l r == -2)    = if (factor r'' l'') >= 0 then rotateL t else rotateL $ Node a l (rotateR r)


balanced d = abs d <= 1
factor l r = (height l) - (height r)

{--
 Tells whether the given tree is and AVL or not. Suppose a tree N with NL it's left son and NR it's right son
 - N is a AVL if : |(hight NL) - (hight NR)| <= 1 and that NL and NR are AVL too
--}
isAVL Empty = True
isAVL (Node x l r) = and [(balanced $ factor l r), isAVL l, isAVL r]

{--
 Helper fonction that creates a AVL tree from a given list of ordered values
--}
fromList :: Ord a => [a] -> Tree a
fromList = L.foldl' insert Empty

{--
 Breadth first traversal
--}
breadth :: [Tree a] -> [a]
breadth [] = []
breadth ts = concatMap (value []) ts ++ breadth (concatMap childs ts)

childs :: Tree a -> [Tree a]
childs Empty = []
childs (Node _ l r) = [l,r]

value :: [a] -> Tree a -> [a]
value acc Empty = acc
value xs (Node x _ _) = xs++[x]

filterT :: (a -> Bool) -> Tree a -> [a]
filterT p t= (filter p) $ breadth [t]

exist:: Eq a => a -> Tree a -> Bool
exist x = not . null . filterT (== x)

mapT:: (a -> b) -> Tree a -> Tree b
mapT f Empty = Empty
mapT f (Node a l r) = Node (f a) (mapT f l) (mapT f r)

instance Functor Tree where
  fmap = mapT

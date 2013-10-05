module Data.Trees.RedBlackTreeV1 where
import qualified Data.List as L hiding (insert)

data Color  = R | B deriving (Eq,Show)
data Tree a = Empty | Node Color !(Tree a) a !(Tree a) deriving (Eq, Show)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = Node R Empty x Empty
insert t x =
  let insert' Empty = Node R Empty x Empty
      insert' s@(Node c l v r)
        | (x == v)  = s
        | (x <  v)  = rebalanceL c (insert' l) v r
        | otherwise = rebalanceR c l v (insert' r)

      Node _ l a r = insert' t
   in Node B l a r

{--
 case B-R-R (root black, left red and left left red)Left-rotation around root
 case B-R-R (root black, left red and left right red) Left-rotation around left and the right rotation around root
--}
rebalanceL :: Color -> Tree a -> a -> Tree a -> Tree a
rebalanceL B (Node R (Node R l v3 r'') v2 r') v1  r = Node R (Node B l v3 r'') v2 (Node B r' v1 r)
rebalanceL B (Node R l v2 (Node R l' v3 r')) v1 r   = Node R (Node B l v2 l') v3 (Node B r' v1 r)
rebalanceL c a x b = Node c a x b

{--
case B-R-R (root black, right red and right right red)Right-rotation around root
case B-R-R (root black, right red and right left red)RIght-rotation around right and then left-rotation around root
--}
rebalanceR :: Color -> Tree a -> a -> Tree a -> Tree a
rebalanceR B l v1 (Node R l' v2 (Node R l'' v3 r)) = Node R (Node B l v1 l') v2 (Node B l'' v3 r)
rebalanceR B l v1 (Node R (Node R l' v3 r') v2 r) = Node R (Node B l v1 l') v3 (Node B r' v2 r)
rebalanceR c a x b = Node c a x b

fromList :: Ord a => [a] -> Tree a
fromList = L.foldl' insert Empty

t1 = fromList [1..4]
t2 = insert t1 5
t3 = insert t2 6
t4 = insert t3 7
t5 = insert t4 8
t6 = insert t5 9

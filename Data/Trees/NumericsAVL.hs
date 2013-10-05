module Data.Trees.NumericsAVL where
import Data.Trees.AVL
import Data.List


-- Take a list and retrun a sorted list.First it converts the list to Tree then convert back the tree to a sorted list
tSort :: Ord a => [a] -> [a]
tSort = toSortedList . fromList


{-- can be implemented as
 sumUp Empty = 0
 sumUp (Node a l r) = a + sumUp l + sumUp r
--}
sumUp = foldl' (+) 0 . toList

--Unsorted Tree
applyT _ (Node a Empty Empty) = a
applyT f (Node a l Empty) = f a (applyT f l)
applyT f (Node a Empty r) = f a (applyT f r)
applyT f (Node a l r) = f a $ f(applyT f l) (applyT f r)

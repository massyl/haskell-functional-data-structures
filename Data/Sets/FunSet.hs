module Data.Sets.FunSet where

type Set a = a -> Bool

newEmpty :: Set a
newEmpty = \_ -> False

add :: Eq a => Set a -> a -> Set a
add s e = \x -> (e == x) || s x

contains ::  Set a -> a -> Bool
contains s e = s e

singleton :: Eq a => a ->Set a
singleton e = \x -> e == x

satisfy :: (Bool -> Bool -> Bool) -> (Set a -> a -> Bool) -> Set a -> Set a -> Set a
satisfy p f s1 s2 = \e -> p (f s1 e) (f s2 e)

union :: Set a -> Set a -> Set a
union = satisfy (||) contains  

intersect :: Set a -> Set a -> Set a
intersect = satisfy (&&) contains

diff :: Set a -> Set a -> Set a
diff s1 s2 = \ e -> s1 e && not (s2 e)

filter' :: (a -> Bool) -> Set a -> Set a
filter' p s = \e -> s e && p e

remove' :: Eq a => a -> Set a -> Set a
remove' e s = \x -> if x == e then False else s x

exists' ::(Num a, Ord a) => Set a -> (a-> Bool) -> Bool
exists' s p = not $ all' s (\x -> not $ p x) 

all' ::(Num a,Ord a) => Set a -> (a -> Bool)-> Bool
all' s p = let iterate i p s max| i >= max = True
                         | contains s i && not(p i) = False      
                         | otherwise = iterate (i + 1) p s max

           in iterate 0 p s 100

map' :: (Bounded a, Num a, Ord a, Eq a, Eq b) => (a -> b)-> Set a -> Set b
map' f s = \y -> exists' s (\x -> f x == y)

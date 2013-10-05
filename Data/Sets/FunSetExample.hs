module Data.Sets.FunSetExample where
import Data.Sets.FunSet

emptySet = newEmpty
set1 = add emptySet (1::Int)
set2 = add set1 (4::Int)
set3 = add set2 (7::Int)
evenSet = add (filter' (\x  -> x `mod` 2 == 0) set3) 8
set4 = remove' 4 set3
mapped = map' (+2) set3
diffs1 = diff set2 evenSet
diffs2 = diff evenSet set3



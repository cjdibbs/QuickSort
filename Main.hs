{-# LANGUAGE TemplateHaskell #-}

import MySort
import Data.List
import Test.QuickCheck.All

prop_qsort_is_sort xs = qsort xs == sort xs
prop_qsort2_is_sort xs = qsort2 xs == sort xs

return [] -- need this for GHC 7.8
-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)

{-# OPTIONS_HADDOCK prune, show-extensions #-}

-- | This module defines the fuzzy set implementation which will be used within
-- the project, as well as some basic operations on them.
module FuzzySet where

import qualified Data.List as L


-- | A 'FuzzySet' is the data type we'll be using to represent fuzzy sets.
-- We use a list of 'Int's, each value represents the degree of membership of
-- the 'Int' index to the fuzzy set as a number between 0 and 10.
-- Discreet lists and integer-only operations were preffered due to
-- implementation convenience, but one can find it trivial to switch to any of the
-- rich <https://goo.gl/qR24GH datakinds> provided by CλaSH.
type FuzzySet = [Int]


-- | 'union' returns the union of two 'FuzzySet's.
union :: FuzzySet -> FuzzySet -> FuzzySet
union = zipWith max

-- | 'intersect' returns the intersection of two 'FuzzySets's.
intersect :: FuzzySet -> FuzzySet -> FuzzySet
intersect = zipWith min

{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# LANGUAGE Trustworthy #-}

-- | This module defines the fuzzy set implementation which will be used within
-- the project, as well as some basic operations on them.
module FuzzySet where

import           CLaSH.Prelude

import           Control.Monad.Reader
import qualified Data.List            as L

import           Config               (Config, confFor)


-- | A 'FuzzySet' is the data type we'll be using to represent fuzzy sets.
-- We use a list of 'Int's, each value represents the degree of membership of
-- the 'Int' index to the fuzzy set as a number between 0 and 10.
-- Discreet lists and integer-only operations were preffered due to
-- implementation convenience, but one can find it trivial to switch to any of the
-- rich <https://goo.gl/qR24GH datakinds> provided by CλaSH.
type FuzzySet = [Int]

-- zeroFuzzySet returns the zero FuzzySet of the given range.
zeroFuzzySet :: Int -> FuzzySet
zeroFuzzySet n = L.replicate n 0

-- | 'unionT' is the function representing the combinational behaviour
-- of a circuit which perform a union of 'FuzzySet's.
unionT :: FuzzySet -> FuzzySet -> FuzzySet
unionT = L.zipWith max

-- | 'union' is the sequential circuit modelling of a 'FuzzySet' unifier.
union :: Reader Config (Signal (FuzzySet, FuzzySet) -> Signal FuzzySet)
union = do
    total <- confFor "totalSpace"
    return $ moore (\_ (i,j) -> unionT i j) id (zeroFuzzySet total)

-- | 'intersectT' returns the intersection of two 'FuzzySet's.
intersectT :: FuzzySet -> FuzzySet -> FuzzySet
intersectT = L.zipWith min

-- | 'intersect' is the sequential circuit modelling a 'FuzzySet' intersector.
intersect :: Reader Config (Signal (FuzzySet, FuzzySet) -> Signal FuzzySet)
intersect = do
    total <- confFor "totalSpace"
    return $ moore (\_ (i, j) -> intersectT i j) id (zeroFuzzySet total)

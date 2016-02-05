-- | This module defines the data structures, helper functions and declarations
-- for the fuzzy logic relations which will be used in out FLC.
module Relation(
        Relation,
        fromSets
        ) where


import           Control.Monad.Reader

import           Config               (Config, confFor)
import           FuzzySet             (FuzzySet)


-- | 'Relation' is the type which represents binary relation in fuzzy logic.
-- It is simply represented as matrix with each value being the degree of
-- membership of the respective (row number, column number) pair (0..100).
-- For convenience, common lists and 'Int's were used. However, switching to
-- another base type (ex. some of the rich <https://goo.gl/qR24GH datakinds>
-- that CλaSH provides in its <http://goo.gl/NfuByM Vec> module.
type Relation = [[Int]]


-- | 'fromSets' is a helper function which defines a 'Relation' based on two
-- input 'FuzzySet's. It is very basic at best, returning a naive
-- representation of what the result might look like.
fromSets :: Reader Config (FuzzySet -> FuzzySet -> Relation)
fromSets = do
    total <- confFor "totalSpace"
    let range = [1..total]

    return (\a b -> map (\n -> map (\m -> a !! n * b !! m) range) range)

-- | 'Speed' is the type representing the discreet
-- set of possible speed "ratings".
-- The meaning of each action can be easier understood by looking
-- at the Show instance for 'SpeedF'.
data Speed = VS | S | N | F | VF
    deriving (Eq, Ord)

instance Show Speed where
    show VS = "Very Slow"
    show S  = "Slow"
    show N  = "Normal"
    show F  = "Fast"
    show VF = "Very Fast"

-- | 'Distance' represents the type of the discreet set
-- of relative distances to the destination.
data Distance = VC | C | M | A | VA
    deriving (Eq, Ord)

instance Show Distance where
    show VC = "Very Close"
    show C  = "Close"
    show M  = "Middle"
    show A  = "Afar"
    show VA = "Very Afar"

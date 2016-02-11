{-# OPTIONS_HADDOCK prune, show-extensions #-}

-- | This module defines the data structures, helper functions and declarations
-- for the fuzzy logic relations which will be used in out FLC.
module Rules where

import           Control.Monad.Reader
import qualified Data.Map             as M

import           Config               (Config, confFor, insert)
import           Fuzzifier            (fuzzifierT)
import           FuzzySet             (FuzzySet, unionT)


-- | 'Action' is the type representing the discreet set of action
-- we might take.
data Action = HardSlowdown | Slowdown | NoOP | Speedup | HardSpeedup
    deriving (Eq, Bounded, Ord, Enum, Show)

-- | 'Speed' is the type representing the discreet
-- set of possible speed "ratings".
data Speed = VerySlow | Slow | Normal | Fast | VeryFast
    deriving (Eq, Bounded, Ord, Enum, Show)

-- | 'Distance' represents the type of the discreet set
-- of relative distances to the destination.
data Distance = VeryClose | Close | Halfway | Far | VeryFar
    deriving (Eq, Bounded, Ord, Enum, Show)

-- | 'Conclusion' is a mapping of 'FuzzySet's to 'Action's.
type Conclusion = M.Map FuzzySet Action

-- | 'conclusion' is the default conclusion.
-- I.e. 'conclusion' == 'zip' ['Action's] Conclusion'Rule'
-- It relies on the "totalSpace", "conclusionSpacing" and "conclusionDelta"
-- Config keys.
conclusion :: Reader Config Conclusion
conclusion = do
    let actRange = ([minBound..maxBound] :: [Action])
    ccDelta <- confFor "conclusionDelta"
    ruler <- local (insert "ruleDelta" ccDelta) mkRuler

    ccSpacing <- confFor "conclusionSpacing"
    termLimit <- local (insert "ruleSpacing" ccSpacing) termLimiter

    return $ M.fromList $ zip ((ruler . termLimit) actRange) actRange

-- | 'TermLimits' is simply the list of upper and lower limit tuples
-- representing a the terms in a 'Rule'.
type TermLimits = [(Int, Int)]

-- | 'termLimiter' takes a list of terms and returns their 'TermLimits'.
-- It relies on the "ruleSpacing" configuration option.
termLimiter :: Reader Config ([a] -> TermLimits)
termLimiter = do
        total <- confFor "totalSpace"
        ruleSpace <- confFor "ruleSpacing"
        return $ \l -> map (\n ->
                        let x = floor (fromIntegral n * (fromIntegral total / (fromIntegral (length l))) :: Double)
                        in (x - ruleSpace, x + ruleSpace)) [1..(length l)]

-- | 'Rule' is a list of the degrees of activation of each premise.
-- (i.e. foldr unionT rule == fuzzy logic rule)
type Rule = [FuzzySet]

-- | 'mkRuler' takes a list of term limits and returns the associated 'Rule'.
-- It relies on the config keys 'totalSpace', 'ruleDelta'.
mkRuler :: Reader Config (TermLimits -> Rule)
mkRuler = do
    total <- confFor "totalSpace"
    ruleDelta <- confFor "ruleDelta"
    fuzz <- local (insert "fuzzificationDelta" ruleDelta) fuzzifierT

    let mkR (s, e) = (unionT (fuzz e) . unionT (fuzz s) . map (\n -> if n > s && n < e then 100 else 0)) [0..total]

    return $ map mkR

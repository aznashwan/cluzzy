-- Contains the definition of the FLC.
module Controller where

import           CLaSH.Prelude        (Signal, moore)

import           Control.Monad.Reader (Reader, local)

import           Config               (Config, confFor, insert)
import           Defuzzifier          (defuzzifierT)
import           Fuzzifier            (fuzzifierT)
import           FuzzySet
import           Rules


-- | 'controllerT' is the function representing the combinational modelling of
-- the FLC. It relies on the "totalSpace", "ruleSpacing", "speedFuzzificationDelta",
-- "distanceFuzzificationDelta" "distanceRuleDelta" and " speedRuleDelta" config keys.
controllerT :: Reader Config (Int -> Int -> Action)
controllerT = do
    total <- confFor "totalSpace"

    -- get the rule generators:
    distRuleDelta <- confFor "distanceRuleDelta"
    distRuler <- local (insert "ruleDelta" distRuleDelta) mkRuler
    speedRuleDelta <- confFor "speedRuleDelta"
    speedRuler <- local (insert "ruleDelta" speedRuleDelta) mkRuler

    -- compute the rules:
    let distTerms = [minBound..maxBound] :: [Distance]
    let speedTerms = [minBound..maxBound] :: [Speed]
    termLimits <- termLimiter
    let distRule = distRuler $ termLimits distTerms
    termLims <- termLimiter
    let speedRule = speedRuler $ termLims speedTerms

    -- instantiate the input fuzzifiers:
    speedFuzzDelta <- confFor "speedInputDelta"
    distFuzzDelta <- confFor "distanceinputDelta"
    speedFuzz <- local (insert "fuzzificationDelta" speedFuzzDelta) fuzzifierT
    distFuzz <- local (insert "fuzzificationDelta" distFuzzDelta) fuzzifierT

    deFuzz <- defuzzifierT
    return (\d s -> deFuzz $ foldr unionT (zeroFuzzySet total) $ intersectT <$> (map (intersectT (distFuzz d)) distRule) <*> (map (intersectT (speedFuzz s)) speedRule))

-- | 'controller' is the sequential circuit representing the FLC.
controller :: Reader Config (Signal (Int, Int) -> Signal Action)
controller = do
    contr <- controllerT
    return $ moore (\_ (i, j) -> contr i j) id NoOP


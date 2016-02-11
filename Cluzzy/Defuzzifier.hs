{-# OPTIONS_HADDOCK prune, show-extensions #-}

-- This module defines the defuzzifier.
module Defuzzifier where

import           Control.Monad.Reader
import qualified Data.Map             as M

import           Config
import           Fuzzifier
import           FuzzySet
import           Rules


-- 'defuzzifierT' is the function mimicking the combination
-- behavior of a defuzifier.
defuzzifierT :: Reader Config (FuzzySet -> Action)
defuzzifierT =
    let
        -- compute weight and moment for center of gravity:
        cg fs = round $ fromIntegral moment / fromIntegral weight
          where (weight, moment) = foldr (\n (w, m) -> let x = fs !! n in (w + x, m + (x * n))) (0, 0) $ [0..length fs - 1]
    in do
        fuzz <- local (insert "fuzzificationDelta" 0) fuzzifierT
        cc <- conclusion

        let mx fs = maximum . intersectT (fuzz $ cg fs)
        let maxim fs = maximum . map (mx fs) $ M.keys cc

        return $ \fs -> (maybe NoOP id . (\k -> M.lookup k cc) . (!! 0) . filter (\f -> mx fs f == (maxim fs))) $ M.keys cc


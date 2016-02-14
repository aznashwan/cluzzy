{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Contains the testbench for our FLC.
-- Configuration options may be found in Config.hs.
-- To run the testbench, please import this module and issue a:
--
-- @
-- sampleN (length testParams) $ expectedOutput (topEntity testInput)
-- @
module Testbenches.FLCTestbench where

import           CLaSH.Prelude

import           Control.Monad.Reader

import           Config               (config)
import           Controller           (controller)
import           Rules


-- | testParams is the list of tuples of test inputs vs. expected output.
testParams :: Vec 16 ((Int, Int), Action)
testParams =
          -- ((distance, speed), expected_action)
          -- balanced situations:
             ((10, 10), HardSlowdown)
          :> ((30, 30), Slowdown)
          :> ((40, 40), NoOP)
          :> ((50, 50), NoOP)
          :> ((60, 60), NoOP)
          :> ((70, 70), Speedup)
          :> ((80, 80), Speedup)
          :> ((90, 90), HardSpeedup)
          -- unbalanced situations:
          -- very close:
          :> ((1, 1), HardSlowdown)
          :> ((99, 99), HardSpeedup) -- still wants to speed up.
          :> ((99, 1), HardSpeedup)
          -- show granularity:
          :> ((1, 19), HardSlowdown)
          :> ((1, 20), HardSlowdown)
          :> ((1, 21), HardSpeedup) -- [0..ruleSpacing] precision
          :> ((2, 21), HardSlowdown) -- maleable
          :> ((2, 22), HardSpeedup)
          :> Nil

topEntity :: Signal (Int, Int) -> Signal Action
topEntity = runReader controller config

testInput :: Signal (Int, Int)
testInput = stimuliGenerator $ map fst testParams

expectedOutput :: Signal Action -> Signal Bool
expectedOutput = outputVerifier $ singleton NoOP ++ map snd testParams


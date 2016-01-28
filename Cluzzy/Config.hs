{-# OPTIONS_HADDOCK show-extensions, prune #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Config defines the global configuration to be passed to all components.
module Config (
        Config,
        config,
        get,
        fromKVList,
        confFor
        ) where


import           CLaSH.Prelude

import           Control.Monad.Reader
import           Data.Default
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)


-- | A Config is just a synonym for a Map String Int
type Config = M.Map String Int

-- !!! THIS IS THE DEFINITION OF THE DEFAULT 'Config' !!! ---
config = fromKVList [
    ("totalSpace", 100),
    ("fuzzificationDelta", 25)
    ]

instance {-# OVERLAPS #-} Default Config where
    def = config

-- | get simply wraps Data.Map.lookup with the default of 0.
get :: String -> Config ->  Int
get k = fromMaybe 0 . M.lookup k

-- | fromKVList wraps Data.Map.fromList.
fromKVList :: [(String, Int)] -> Config
fromKVList = M.fromList

-- | confFor is a convenience method for asking for the given
-- Config key in a Reader.
confFor :: String -> Reader Config Int
confFor s = asks (get s)

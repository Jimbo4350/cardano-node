{-# LANGUAGE CPP #-}

module Cardano.Logging.Resources
    (
      ResourceStats(..)
    , readResourceStats
    ) where


import           Cardano.Logging.Resources.Types
#if defined(linux_HOST_OS)
import qualified Cardano.Logging.Resources.Linux as Platform
#elif defined(mingw32_HOST_OS)
import qualified Cardano.Logging.Resources.Windows as Platform
#elif defined(darwin_HOST_OS)
import qualified Cardano.Logging.Resources.Darwin as Platform
#else
import qualified Cardano.Logging.Resources.Dummy as Platform
#endif


readResourceStats :: IO (Maybe ResourceStats)
readResourceStats = Platform.readRessoureStatsInternal

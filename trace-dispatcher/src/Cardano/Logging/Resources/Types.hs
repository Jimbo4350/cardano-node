{-# LANGUAGE CPP #-}

module Cardano.Logging.Resources.Types
    (
      ResourceStats(..)
    , docResourceStats
    ) where


import           Cardano.Logging.Types
import           Data.Aeson (Value (Number, String), (.=))
import           Data.Text (pack)
import           Data.Word

-- | Struct for resources used by the process
data ResourceStats
  = ResourceStats
      { rCentiCpu   :: !Word64
      , rCentiGC    :: !Word64
      , rCentiMut   :: !Word64
      , rGcsMajor   :: !Word64
      , rGcsMinor   :: !Word64
      , rAlloc      :: !Word64
      , rLive       :: !Word64
      , rRSS        :: !Word64
      , rCentiBlkIO :: !Word64
      , rThreads    :: !Word64
      }
  deriving (Show)

docResourceStats :: Documented (ResourceStats)
docResourceStats = Documented [
      DocMsg
        (ResourceStats 1 1 1 1 1 1 1 1 1 1)
        [(["Stat","Cputicks"], "Reports the CPU ticks, sice the process was started")
        ,(["Mem","Resident"], "TODO JNF")
        ,(["RTS","GcLiveBytes"],"TODO JNF")
        ,(["RTS","GcMajorNum"],"TODO JNF")
        ,(["RTS","GcMinorNum"],"TODO JNF")
        ,(["RTS","Gcticks"],"TODO JNF")
        ,(["RTS","Mutticks"],"TODO JNF")
        ,(["RTS","Threads"],"TODO JNF")
        ]
        "TODO JNF"
    ]

instance LogFormatting ResourceStats where
    forHuman rs = "Resources: CpuTicks " <> (pack . show) (rCentiCpu rs)
                  <> ", Resident " <> (pack . show) (rRSS rs)
                  <> ", GcLiveBytes " <> (pack . show) (rLive rs)
                  <> ", GcMajorNum " <> (pack . show) (rGcsMajor rs)
                  <> ", GcMinorNum " <> (pack . show) (rGcsMinor rs)
                  <> ", Gcticks " <> (pack . show) (rCentiGC rs)
                  <> ", Mutticks " <> (pack . show) (rCentiMut rs)
                  <> ", Threads " <> (pack . show) (rThreads rs)
                  <> "."

    forMachine _dtal rs = mkObject
      [ "kind"          .= String "ResourceStats"
      , "Cputicks"      .= Number (fromIntegral $ rCentiCpu rs)
      , "Resident"      .= Number (fromIntegral $ rRSS rs)
      , "GcLiveBytes"   .= Number (fromIntegral $ rLive rs)
      , "GcMajorNum"    .= Number (fromIntegral $ rGcsMajor rs)
      , "GcMinorNum"    .= Number (fromIntegral $ rGcsMinor rs)
      , "Gcticks"       .= Number (fromIntegral $ rCentiGC rs)
      , "Mutticks"      .= Number (fromIntegral $ rCentiMut rs)
      , "Threads"       .= Number (fromIntegral $ rThreads rs)
      ]

    asMetrics rs = [
        IntM ["Stat","Cputicks"] (fromIntegral $ rCentiCpu rs)
      , IntM ["Mem","Resident"] (fromIntegral $ rRSS rs)
      , IntM ["RTS","GcLiveBytes"] (fromIntegral $ rLive rs)
      , IntM ["RTS","GcMajorNum"] (fromIntegral $ rGcsMajor rs)
      , IntM ["RTS","GcMinorNum"] (fromIntegral $ rGcsMinor rs)
      , IntM ["RTS","Gcticks"] (fromIntegral $ rCentiGC rs)
      , IntM ["RTS","Mutticks"] (fromIntegral $ rCentiMut rs)
      , IntM ["Stat","Threads"] (fromIntegral $ rThreads rs)
      ]

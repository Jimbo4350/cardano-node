
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Logging.Counters.Windows
    (
    , readRessoureStatsInternal
    ) where

#ifdef ENABLE_OBSERVABLES
import           Data.Foldable (foldrM)
-- import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.Stats as GhcStats
import           System.Win32.Process (ProcessId, getCurrentProcessId)
import           System.Win32.Types


-- use PsAPI version 2
#define PSAPI_VERSION 2

#include <windows.h>
#include <psapi.h>

#include "os-support-win.h"


{- type aliases -}
type ULONGLONG = Word64

{- memory information -}

{- https://docs.microsoft.com/de-de/windows/win32/api/psapi/ns-psapi-process_memory_counters
typedef struct _PROCESS_MEMORY_COUNTERS {
  DWORD  cb;
  DWORD  PageFaultCount;
  SIZE_T PeakWorkingSetSize;
  SIZE_T WorkingSetSize;
  SIZE_T QuotaPeakPagedPoolUsage;
  SIZE_T QuotaPagedPoolUsage;
  SIZE_T QuotaPeakNonPagedPoolUsage;
  SIZE_T QuotaNonPagedPoolUsage;
  SIZE_T PagefileUsage;
  SIZE_T PeakPagefileUsage;
} PROCESS_MEMORY_COUNTERS; -}

data ProcessMemoryCounters = ProcessMemoryCounters
  { _cb :: DWORD
  , _pageFaultCount :: DWORD
  , _peakWorkingSetSize :: SIZE_T
  , _workingSetSize :: SIZE_T
  , _quotaPeakPagedPoolUsage :: SIZE_T
  , _quotaPagedPoolUsage :: SIZE_T
  , _quotaPeakNonPagedPoolUsage :: SIZE_T
  , _quotaNonPagedPoolUsage :: SIZE_T
  , _pagefileUsage :: SIZE_T
  , _peakPagefileUsage :: SIZE_T
  }

instance Storable ProcessMemoryCounters where
  alignment _ = #const offsetof(struct {char x__; PROCESS_MEMORY_COUNTERS (y__); }, y__)
  sizeOf _    = #size PROCESS_MEMORY_COUNTERS
  peek ptr    = ProcessMemoryCounters
                <$> (#peek PROCESS_MEMORY_COUNTERS, cb) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, PageFaultCount) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, PeakWorkingSetSize) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, WorkingSetSize) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, QuotaPeakPagedPoolUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, QuotaPagedPoolUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, QuotaPeakNonPagedPoolUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, QuotaNonPagedPoolUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, PagefileUsage) ptr
                <*> (#peek PROCESS_MEMORY_COUNTERS, PeakPagefileUsage) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_process_memory_info :: Ptr ProcessMemoryCounters -> CInt -> IO CInt

{- I/O counters -}
{- https://docs.microsoft.com/de-de/windows/win32/api/winnt/ns-winnt-io_counters
typedef struct _IO_COUNTERS {
  ULONGLONG ReadOperationCount;
  ULONGLONG WriteOperationCount;
  ULONGLONG OtherOperationCount;
  ULONGLONG ReadTransferCount;
  ULONGLONG WriteTransferCount;
  ULONGLONG OtherTransferCount;
} IO_COUNTERS; -}

data IOCounters = IOCounters
  { _readOperationCount :: ULONGLONG
  , _writeOperationCount :: ULONGLONG
  , _otherOperationCount :: ULONGLONG
  , _readTransferCount :: ULONGLONG
  , _writeTransferCount :: ULONGLONG
  , _otherTransferCount :: ULONGLONG
  }

instance Storable IOCounters where
  alignment _ = #const offsetof(struct {char x__; IO_COUNTERS (y__); }, y__)
  sizeOf _    = #size IO_COUNTERS
  peek ptr    = IOCounters
                <$> (#peek IO_COUNTERS, ReadOperationCount) ptr
                <*> (#peek IO_COUNTERS, WriteOperationCount) ptr
                <*> (#peek IO_COUNTERS, OtherOperationCount) ptr
                <*> (#peek IO_COUNTERS, ReadTransferCount) ptr
                <*> (#peek IO_COUNTERS, WriteTransferCount) ptr
                <*> (#peek IO_COUNTERS, OtherTransferCount) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_io_counters :: Ptr IOCounters -> CInt -> IO CInt


{- system information -}
  {-
typedef struct _SYSTEM_INFO {
    union {
        DWORD dwOemId;          // Obsolete field...do not use
        struct {
            WORD wProcessorArchitecture;
            WORD wReserved;
        } DUMMYSTRUCTNAME;
    } DUMMYUNIONNAME;
    DWORD dwPageSize;
    LPVOID lpMinimumApplicationAddress;
    LPVOID lpMaximumApplicationAddress;
    DWORD_PTR dwActiveProcessorMask;
    DWORD dwNumberOfProcessors;
    DWORD dwProcessorType;
    DWORD dwAllocationGranularity;
    WORD wProcessorLevel;
    WORD wProcessorRevision;
} SYSTEM_INFO, *LPSYSTEM_INFO; -}

data SystemInfo = SystemInfo
  { --_dummy :: DWORD
    _dwPageSize :: DWORD
  --, _lpMinimumApplicationAddress :: LPVOID
  --, _lpMaximumApplicationAddress :: LPVOID
  --, _dwActiveProcessorMask :: DWORD_PTR
  , _dwNumberOfProcessors :: DWORD
  , _dwProcessorType :: DWORD
  , _dwAllocationGranularity :: DWORD
  , _wProcessorLevel :: WORD
  , _wProcessorRevision :: WORD
  }

instance Storable SystemInfo where
  alignment _ = #const offsetof(struct {char x__; SYSTEM_INFO (y__); }, y__)
  sizeOf _    = #size SYSTEM_INFO
  peek ptr    = SystemInfo
                <$> (#peek SYSTEM_INFO, dwPageSize) ptr
                -- <*> (#peek SYSTEM_INFO, lpMinimumApplicationAddress) ptr
                -- <*> (#peek SYSTEM_INFO, lpMaximumApplicationAddress) ptr
                -- <*> (#peek SYSTEM_INFO, dwActiveProcessorMask) ptr
                <*> (#peek SYSTEM_INFO, dwNumberOfProcessors) ptr
                <*> (#peek SYSTEM_INFO, dwProcessorType) ptr
                <*> (#peek SYSTEM_INFO, dwAllocationGranularity) ptr
                <*> (#peek SYSTEM_INFO, wProcessorLevel) ptr
                <*> (#peek SYSTEM_INFO, wProcessorRevision) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_system_info:: Ptr SystemInfo -> IO CInt

data CpuTimes = CpuTimes {
    usertime :: ULONGLONG
  , systime :: ULONGLONG
  , idletime :: ULONGLONG
  }

instance Storable CpuTimes where
  alignment _ = #const offsetof(struct {char x__; CPU_TIMES (y__); }, y__)
  sizeOf _    = #size CPU_TIMES
  peek ptr    = CpuTimes
                <$> (#peek CPU_TIMES, usertime) ptr
                <*> (#peek CPU_TIMES, systime) ptr
                <*> (#peek CPU_TIMES, idletime) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_sys_cpu_times :: Ptr CpuTimes -> IO CInt
foreign import ccall unsafe c_get_proc_cpu_times :: Ptr CpuTimes -> CInt -> IO CInt

foreign import ccall unsafe c_get_win_bits :: CInt -> IO CInt

{-  -}

readCounters :: SubTrace -> IO [Counter]
readCounters NoTrace                   = return []
readCounters Neutral                   = return []
readCounters (TeeTrace _)              = return []
readCounters (FilterTrace _)           = return []
readCounters UntimedTrace              = return []
readCounters DropOpening               = return []
readCounters (SetSeverity _)           = return []
#ifdef ENABLE_OBSERVABLES
readCounters (ObservableTraceSelf tts) = do
    pid <- getCurrentProcessId
    takeMeasurements pid tts
readCounters (ObservableTrace _pid _tts) = return []

takeMeasurements :: ProcessId -> [ObservableInstance] -> IO [Counter]
takeMeasurements pid tts =
    foldrM (\(sel, fun) a ->
       if any (== sel) tts
       then (fun >>= \xs -> return $ a ++ xs)
       else return a) [] selectors
  where
    selectors = [ (MonotonicClock, getMonoClock)
                , (SysStats, readSysStats pid)
                , (MemoryStats, readProcMem pid)
                , (ProcessStats, readProcStats pid)
                , (NetStats, readProcNet pid)
                , (IOStats, readProcIO pid)
                , (GhcRtsStats, readRTSStats)
                ]
#else
readCounters (ObservableTraceSelf _)   = return []
readCounters (ObservableTrace     _ _) = return []
#endif


#ifdef ENABLE_OBSERVABLES
readProcMem :: ProcessId -> IO [Counter]
readProcMem pid = do
    meminfo <- getMemoryInfo pid
    return [ Counter MemoryCounter "Pid" (PureI $ fromIntegral pid)
           , Counter MemoryCounter "WorkingSetSize" (PureI $ fromIntegral (_workingSetSize meminfo))
           , Counter MemoryCounter "PeakWorkingSetSize" (PureI $ fromIntegral (_peakWorkingSetSize meminfo))
           , Counter MemoryCounter "QuotaPeakPagedPoolUsage" (PureI $ fromIntegral (_quotaPeakPagedPoolUsage meminfo))
           , Counter MemoryCounter "QuotaPagedPoolUsage" (PureI $ fromIntegral (_quotaPagedPoolUsage meminfo))
           , Counter MemoryCounter "QuotaPeakNonPagedPoolUsage" (PureI $ fromIntegral (_quotaPeakNonPagedPoolUsage meminfo))
           , Counter MemoryCounter "QuotaNonPagedPoolUsage" (PureI $ fromIntegral (_quotaNonPagedPoolUsage meminfo))
           , Counter MemoryCounter "PagefileUsage" (PureI $ fromIntegral (_pagefileUsage meminfo))
           , Counter MemoryCounter "PeakPagefileUsage" (PureI $ fromIntegral (_peakPagefileUsage meminfo))
           , Counter MemoryCounter "PageFaultCount" (PureI $ fromIntegral (_pageFaultCount meminfo))
           ]

getMemoryInfo :: ProcessId -> IO ProcessMemoryCounters
getMemoryInfo pid =
  allocaBytes 128 $ \ptr -> do
    res <- c_get_process_memory_info ptr (fromIntegral pid)
    if res <= 0
      then do
        putStrLn $ "c_get_process_memory_info: failure returned: " ++ (show res)
        return $ ProcessMemoryCounters 0 0 0 0 0 0 0 0 0 0
      else
        peek ptr
#endif


#ifdef ENABLE_OBSERVABLES
readSysStats :: ProcessId -> IO [Counter]
readSysStats pid = do
    sysinfo <- getSysInfo
    cputimes <- getSysCpuTimes
    winbits <- getWinBits
    return [ Counter SysInfo "Pid" (PureI $ fromIntegral pid)
           , Counter SysInfo "Platform" (PureI $ fromIntegral $ fromEnum Windows)
           , Counter SysInfo "CpuCount" (PureI $ fromIntegral $ _dwNumberOfProcessors sysinfo)
           , Counter SysInfo "PageSize" (PureI $ fromIntegral $ _dwPageSize sysinfo)
           , Counter SysInfo "ProcessorType" (PureI $ fromIntegral $ _dwProcessorType sysinfo)
           , Counter SysInfo "AllocationGranularity" (PureI $ fromIntegral $ _dwAllocationGranularity sysinfo)
           , Counter SysInfo "ProcessorLevel" (PureI $ fromIntegral $ _wProcessorLevel sysinfo)
           , Counter SysInfo "ProcessorRevision" (PureI $ fromIntegral $ _wProcessorRevision sysinfo)
           , Counter SysInfo "SysUserTime" (Microseconds $ usertime cputimes)
           , Counter SysInfo "KernelTime" (Microseconds $ systime cputimes)
           , Counter SysInfo "CPUTime" (Microseconds $ (systime cputimes + usertime cputimes))
           , Counter SysInfo "IdleTime" (Microseconds $ idletime cputimes)
           , Counter SysInfo "WindowsPlatformBits" (PureI $ fromIntegral winbits)
           ]
  where
    getWinBits :: IO CInt
    getWinBits = c_get_win_bits (fromIntegral pid)
    getSysCpuTimes :: IO CpuTimes
    getSysCpuTimes =
      allocaBytes 128 $ \ptr -> do
        res <- c_get_sys_cpu_times ptr
        if res <= 0
          then do
            putStrLn $ "c_get_sys_cpu_times: failure returned: " ++ (show res)
            return $ CpuTimes 0 0 0
          else
            peek ptr
    getSysInfo :: IO SystemInfo
    getSysInfo =
      allocaBytes 128 $ \ptr -> do
        res <- c_get_system_info ptr
        if res <= 0
          then do
            putStrLn $ "c_get_system_info: failure returned: " ++ (show res)
            return $ SystemInfo 0 0 0 0 0 0
          else
            peek ptr

#endif


#ifdef ENABLE_OBSERVABLES
readRessoureStatsInternal :: IO (Maybe ResourceStats)
readRessoureStatsInternal = getCurrentProcessId >>= \pid -> do
  cpu <- getCpuTimes   pid
  mem <- getMemoryInfo pid
  rts <- GhcStats.getRTSStats
  pure . Just $
    Resources
    { rCentiCpu   = usecsToCenti $ usertime cpu + systime cpu
    , rCentiGC    = nsToCenti $ GhcStats.gc_cpu_ns rts
    , rCentiMut   = nsToCenti $ GhcStats.mutator_cpu_ns rts
    , rGcsMajor   = fromIntegral $ GhcStats.major_gcs rts
    , rGcsMinor   = fromIntegral $ GhcStats.gcs rts - GhcStats.major_gcs rts
    , rAlloc      = GhcStats.allocated_bytes rts
    , rLive       = GhcStats.gcdetails_live_bytes $ GhcStats.gc rts
    , rRSS        = fromIntegral (_workingSetSize mem)
    , rCentiBlkIO = 0
    , rThreads    = 0
    }
 where
   usecsToCenti :: ULONGLONG -> Word64
   usecsToCenti = fromIntegral . (`div` 10000)
   nsToCenti :: GhcStats.RtsTime -> Word64
   nsToCenti = fromIntegral . (`div` 10000000)

readProcStats :: ProcessId -> IO [Counter]
readProcStats pid = do
    cputimes <- getCpuTimes pid
    return [ Counter StatInfo "Pid" (PureI $ fromIntegral pid)
           , Counter StatInfo "UserTime" (Microseconds $ usertime cputimes)
           , Counter StatInfo "SystemTime" (Microseconds $ systime cputimes)
           , Counter StatInfo "StartTime" (Microseconds $ idletime cputimes)
           , Counter StatInfo "CPUTime" (Microseconds $ (systime cputimes + usertime cputimes))
           ]

getCpuTimes :: ProcessId -> IO CpuTimes
getCpuTimes pid =
  allocaBytes 128 $ \ptr -> do
    res <- c_get_proc_cpu_times ptr (fromIntegral pid)
    if res <= 0
      then do
        putStrLn $ "c_get_proc_cpu_times: failure returned: " ++ (show res)
        return $ CpuTimes 0 0 0
      else
        peek ptr

#endif


#ifdef ENABLE_OBSERVABLES
readProcIO :: ProcessId -> IO [Counter]
readProcIO pid = do
    ioinfo <- getIOInfo
    return [ Counter IOCounter "Pid" (PureI $ fromIntegral pid)
           , Counter IOCounter "ReadTransferCount" (Bytes $ fromIntegral (_readTransferCount ioinfo))
           , Counter IOCounter "WriteTransferCount" (Bytes $ fromIntegral (_writeTransferCount ioinfo))
           , Counter IOCounter "OtherTransferCount" (Bytes $ fromIntegral (_otherTransferCount ioinfo))
           , Counter IOCounter "ReadOperationCount" (PureI $ fromIntegral (_readOperationCount ioinfo))
           , Counter IOCounter "WriteOperationCount" (PureI $ fromIntegral (_writeOperationCount ioinfo))
           , Counter IOCounter "OtherOperationCount" (PureI $ fromIntegral (_otherOperationCount ioinfo))
           ]
  where
    getIOInfo :: IO IOCounters
    getIOInfo =
      allocaBytes 128 $ \ptr -> do
        res <- c_get_io_counters ptr (fromIntegral pid)
        if res <= 0
          then do
            putStrLn $ "c_get_io_counters: failure returned: " ++ (show res)
            return $ IOCounters 0 0 0 0 0 0
          else
            peek ptr


#endif


#ifdef ENABLE_OBSERVABLES
readProcNet :: ProcessId -> IO [Counter]
readProcNet _pid = pure []
#endif

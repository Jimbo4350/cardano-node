
typedef struct _CPU_TIMES {
  ULONGLONG usertime;
  ULONGLONG systime;
  ULONGLONG idletime;
} CPU_TIMES;

int c_get_process_memory_info (PROCESS_MEMORY_COUNTERS *counters, DWORD pid);
int c_get_io_counters (IO_COUNTERS *counters, DWORD pid);
int c_get_sys_cpu_times (CPU_TIMES *cputimes);
int c_get_proc_cpu_times (CPU_TIMES *cputimes, DWORD pid);
int c_get_system_info (SYSTEM_INFO *sysinfo);
int c_get_win_bits (DWORD pid);


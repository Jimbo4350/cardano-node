#include <mach/task_info.h>
#include <mach/host_info.h>
#include <net/if.h>
#include <net/if_var.h>

typedef struct _CPU_TIMES {
  int64_t usertime;
  int64_t systime;
  int64_t idletime;
  int64_t nicetime;
} CPU_TIMES;

#define MAX_NET_IO 32
typedef struct _NET_IO {
    u_int32_t nifs;
    char* ifnames[MAX_NET_IO];
    struct if_data64 ifs[MAX_NET_IO];
} NET_IO;

typedef struct _DISK_INFO {
    int64_t reads, writes;
    int64_t read_bytes, write_bytes;
    int64_t read_time, write_time;  // nanoseconds
} DISK_INFO;
#define MAX_DISK_COUNTERS 32
typedef struct _DISK_COUNTERS {
    u_int32_t ndsks;
    char* dsknames[MAX_DISK_COUNTERS];
    DISK_INFO dsks[MAX_DISK_COUNTERS];
} DISK_COUNTERS;

int c_get_process_memory_info (struct mach_task_basic_info *counters, int pid);
//int c_get_host_info (struct host_basic_info *counters);
long c_get_boot_time();
int c_get_sys_cpu_times(CPU_TIMES *counters);
int c_get_sys_network_io_counters(NET_IO *counters);
int c_get_sys_disk_io_counters(DISK_COUNTERS *counters);
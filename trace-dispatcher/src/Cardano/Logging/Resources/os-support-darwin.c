
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <mach/mach.h>
#include <mach/task.h>
#include <mach/task_info.h>
#include <mach/mach_init.h>
#include <mach/mach_host.h>
#include <net/route.h>
#include <net/if_dl.h>

/*
// includes for c_get_sys_disk_io_counters
// will require GHC options in cabal file:
//   if os(darwin)
//     ghc-options:       -framework CoreFoundation -framework IOKit
//
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/storage/IOBlockStorageDriver.h>
#include <IOKit/storage/IOMedia.h>
#include <IOKit/IOBSD.h>
#include <IOKit/ps/IOPowerSources.h>
#include <IOKit/ps/IOPSKeys.h>
*/
#include "os-support-darwin.h"


/* c_get_process_memory_info */

int c_get_process_memory_info(struct mach_task_basic_info *counters, int pid)
{
    task_t task = MACH_PORT_NULL;
    if (task_for_pid(current_task(), pid, &task) != KERN_SUCCESS) {
        return -2;
    }
    struct mach_task_basic_info t_info;
    mach_msg_type_number_t t_info_count = MACH_TASK_BASIC_INFO_COUNT;
    if (task_info(task, MACH_TASK_BASIC_INFO, (task_info_t)counters, &t_info_count) != KERN_SUCCESS) {
        return -1;
    }
    return 1;
}


/* c_get_host_info */
/* currently this is not used
int c_get_host_info(struct host_basic_info *counters)
{
    mach_msg_type_number_t count = HOST_BASIC_INFO_COUNT;
    mach_port_t host_port = mach_host_self();
    if (host_statistics(host_port, HOST_BASIC_INFO,
                            (host_info_t)counters, &count) != KERN_SUCCESS) {
        return -2;
    }
    mach_port_deallocate(mach_task_self(), host_port);
    return 1;
} */

/* c_get_boot_time */

long c_get_boot_time()
{
    // copied from psutil
    // fetch sysctl "kern.boottime"
    static int request[2] = { CTL_KERN, KERN_BOOTTIME };
    struct timeval result;
    size_t result_len = sizeof result;
    time_t boot_time = 0;

    if (sysctl(request, 2, &result, &result_len, NULL, 0) == -1) {
        return -1;
    }
    return result.tv_sec;
}

/* c_get_sys_cpu_times */
int c_get_sys_cpu_times(CPU_TIMES *counters)
{
    mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
    host_cpu_load_info_data_t r_load;

    mach_port_t host_port = mach_host_self();
    if (host_statistics(host_port, HOST_CPU_LOAD_INFO,
                            (host_info_t)&r_load, &count) != KERN_SUCCESS) {
        return -2;
    }
    mach_port_deallocate(mach_task_self(), host_port);
    counters->usertime = r_load.cpu_ticks[CPU_STATE_USER] * 100000 / CLK_TCK;
    counters->systime = r_load.cpu_ticks[CPU_STATE_SYSTEM] * 100000 / CLK_TCK;
    counters->idletime = r_load.cpu_ticks[CPU_STATE_IDLE] * 100000 / CLK_TCK;
    counters->nicetime = r_load.cpu_ticks[CPU_STATE_NICE] * 100000 / CLK_TCK;
    return 1;
}

/* c_get_proc_cpu_times */



/* c_get_sys_disk_io_counters */
/* adapted from psutil */
int c_get_sys_disk_io_counters(DISK_COUNTERS *counters) {
    counters->ndsks = 0;
    // uncomment the following to extract disk I/O metrics
    // requires to include the right headers (see top of this file)
    // and link to frameworks on Darwin (also described there)
/*
    int noutput = 0;
    CFDictionaryRef parent_dict;
    CFDictionaryRef props_dict;
    CFDictionaryRef stats_dict;
    io_registry_entry_t parent;
    io_registry_entry_t disk;
    io_iterator_t disk_list;

    // Get list of disks
    if (IOServiceGetMatchingServices(
            kIOMasterPortDefault,
            IOServiceMatching(kIOMediaClass),
            &disk_list) != kIOReturnSuccess) {
        return -4;
    }

    // Iterate over disks
    while ((disk = IOIteratorNext(disk_list)) != 0) {
        parent_dict = NULL;
        props_dict = NULL;
        stats_dict = NULL;

        if (IORegistryEntryGetParentEntry(disk, kIOServicePlane, &parent) != kIOReturnSuccess) {
            return -3;
        }

        if (IOObjectConformsTo(parent, "IOBlockStorageDriver")) {

            if (IORegistryEntryCreateCFProperties(disk,
                    (CFMutableDictionaryRef *) &parent_dict,
                    kCFAllocatorDefault, kNilOptions) != kIOReturnSuccess) {
                IOObjectRelease(disk);
                IOObjectRelease(parent);
                return -2;
            }

            if (IORegistryEntryCreateCFProperties(parent,
                    (CFMutableDictionaryRef *) &props_dict,
                    kCFAllocatorDefault, kNilOptions) != kIOReturnSuccess) {
                CFRelease(props_dict);
                IOObjectRelease(disk);
                IOObjectRelease(parent);
                return -1;
            }

            const int kMaxDiskNameSize = 64;
            char disk_name[kMaxDiskNameSize+1]; memset(disk_name, 0, kMaxDiskNameSize+1);
            CFStringRef disk_name_ref = (CFStringRef)
                CFDictionaryGetValue(parent_dict, CFSTR(kIOBSDNameKey));
            CFStringGetCString(disk_name_ref, disk_name,
                               kMaxDiskNameSize,
                               CFStringGetSystemEncoding());
            counters->dsknames[noutput] = strdup(disk_name);

            stats_dict = (CFDictionaryRef)
                CFDictionaryGetValue(props_dict, CFSTR(kIOBlockStorageDriverStatisticsKey));
            if (stats_dict == NULL) {
                continue;
            }
            CFNumberRef number;
            int64_t t_num64 = 0;
            // Get disk reads/writes
            if ((number = (CFNumberRef)CFDictionaryGetValue(stats_dict,
                    CFSTR(kIOBlockStorageDriverStatisticsReadsKey)))) {
                CFNumberGetValue(number, kCFNumberSInt64Type, &t_num64);
                counters->dsks[noutput].reads = t_num64;
            }
            if ((number = (CFNumberRef)CFDictionaryGetValue(stats_dict,
                    CFSTR(kIOBlockStorageDriverStatisticsWritesKey)))) {
                CFNumberGetValue(number, kCFNumberSInt64Type, &t_num64);
                counters->dsks[noutput].writes = t_num64;
            }
            // Get disk bytes read/written
            if ((number = (CFNumberRef)CFDictionaryGetValue(stats_dict,
                    CFSTR(kIOBlockStorageDriverStatisticsBytesReadKey)))) {
                CFNumberGetValue(number, kCFNumberSInt64Type, &t_num64);
                counters->dsks[noutput].read_bytes = t_num64;
            }
            if ((number = (CFNumberRef)CFDictionaryGetValue(stats_dict,
                    CFSTR(kIOBlockStorageDriverStatisticsBytesWrittenKey)))) {
                CFNumberGetValue(number, kCFNumberSInt64Type, &t_num64);
                counters->dsks[noutput].write_bytes = t_num64;
            }
            // Get disk time spent reading/writing (nanoseconds)
            if ((number = (CFNumberRef)CFDictionaryGetValue(stats_dict,
                    CFSTR(kIOBlockStorageDriverStatisticsTotalReadTimeKey)))) {
                CFNumberGetValue(number, kCFNumberSInt64Type, &t_num64);
                counters->dsks[noutput].read_time = t_num64;
            }
            if ((number = (CFNumberRef)CFDictionaryGetValue(stats_dict,
                    CFSTR(kIOBlockStorageDriverStatisticsTotalWriteTimeKey)))) {
                CFNumberGetValue(number, kCFNumberSInt64Type, &t_num64);
                counters->dsks[noutput].write_time = t_num64;
            }

            CFRelease(parent_dict);
            IOObjectRelease(parent);
            CFRelease(props_dict);
            IOObjectRelease(disk);

            noutput++;
            if (noutput >= MAX_DISK_COUNTERS) { break; }
        }
    } // while
    IOObjectRelease (disk_list);
    counters->ndsks = noutput;
*/
    return 1;
}

/* c_get_sys_network_io_counters */
/* adapted from psutil */
int c_get_sys_network_io_counters(NET_IO *counters) {
    counters->nifs = 0;
    int noutput = 0;
    char *msghdrbuf = NULL, *end_of_list, *next;
    struct if_msghdr *ifm;
    int mib[6];
    mib[0] = CTL_NET;          // networking subsystem
    mib[1] = PF_ROUTE;         // type of information
    mib[2] = 0;                // protocol (IPPROTO_xxx)
    mib[3] = 0;                // address family
    mib[4] = NET_RT_IFLIST2;   // operation
    mib[5] = 0;
    size_t buflen;
    if (sysctl(mib, 6, NULL, &buflen, NULL, 0) < 0) {
        return -4;
    }

    msghdrbuf = malloc(buflen);
    if (msghdrbuf == NULL) {
        return -3;
    }
    if (sysctl(mib, 6, msghdrbuf, &buflen, NULL, 0) < 0) {
        return -2;
    }

    char nmbuf[12+1];

    end_of_list = msghdrbuf + buflen;
    for (next = msghdrbuf; next < end_of_list; ) {
        ifm = (struct if_msghdr *)next;
        next += ifm->ifm_msglen;
        if (ifm->ifm_type == RTM_IFINFO2 && (ifm->ifm_flags & IFF_UP)) {
            struct if_msghdr2 *if2m = (struct if_msghdr2 *)ifm;
            // access name of interface and make a copy of the string
            // struct sockaddr_dl *sdl = (struct sockaddr_dl *)(if2m + 1);
            // memset(nmbuf, 0, 12+1);
            // strncpy(nmbuf, sdl->sdl_data, MIN(12,sdl->sdl_nlen));
            // counters->ifnames[noutput] = strdup(nmbuf);

            // copy struct if_data64
            memcpy(&counters->ifs[noutput], &if2m->ifm_data, sizeof(struct if_data64));
            noutput++;
        }
        if (noutput >= MAX_NET_IO) { break; }
    }
    counters->nifs = noutput;
    free(msghdrbuf);
    return 1;
}

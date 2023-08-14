/*
 * Common header shared by kernel and userspace apps
 * */

// The 'magic' number for our driver, this is used to identify a device
#define IOCTL_LLKD_MAGIC 0xA8

#define IOCTL_LLKD_MAXIOCTL 3
/*
 * The _IO{R|W}() macros can be summarized as follows:
_IO(type,nr)                  ioctl command with no argument
_IOR(type,nr,datatype)        ioctl command for reading data from the kernel/drv
_IOW(type,nr,datatype)        ioctl command for writing data to the kernel/drv
_IOWR(type,nr,datatype)       ioctl command for read/write transfers

By using these macros, the direction part (two bits) are set properly
*/
/* our dummy ioctl (IOC) RESET command */
#define IOCTL_LLKD_IOCRESET _IO(IOCTL_LLKD_MAGIC, 0)

/* our dummy ioctl (IOC) Query POWER command */
#define IOCTL_LLKD_IOCQPOWER _IOR(IOCTL_LLKD_MAGIC, 1, int)

/* our dummy ioctl (IOC) Set POWER command */
#define IOCTL_LLKD_IOCSPOWER _IOW(IOCTL_LLKD_MAGIC, 2, int)

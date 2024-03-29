#ifndef __LLKD_SED_COMMON_H__
#include "../../convenient.h"

/* The 'magic' number for our driver; see
 * Documentation/ioctl/ioctl-number.rst
 * Of course, we don't know for _sure_ if the magic # we choose here this
 * will remain free; it really doesn't matter, this is just for demo purposes;
 * don't try and upstream this without further investigation :-)
 */
#define IOCTL_LLKD_SED_MAGIC 0xA9
#define IOCTL_LLKD_SED_MAXIOCTL 4
/*
 * The _IO{R|W}() macros can be summarized as follows:
_IO(type,nr)                  ioctl command with no argument
_IOR(type,nr,datatype)        ioctl command for reading data from the kernel/drv
_IOW(type,nr,datatype)        ioctl command for writing data to the kernel/drv
_IOWR(type,nr,datatype)       ioctl command for read/write transfers
*/
/* our ioctl (IOC) encrypt message command */
#define IOCTL_LLKD_SED_IOC_ENCRYPT_MSG _IOR(IOCTL_LLKD_SED_MAGIC, 1, int)
/* our ioctl (IOC) retrieve message command */
#define IOCTL_LLKD_SED_IOC_RETRIEVE_MSG _IOR(IOCTL_LLKD_SED_MAGIC, 2, int)
/* our ioctl (IOC) decrypt message command */
#define IOCTL_LLKD_SED_IOC_DECRYPT_MSG _IOR(IOCTL_LLKD_SED_MAGIC, 3, int)
/* our ioctl (IOC) destroy message command */
#define IOCTL_LLKD_SED_IOC_DESTROY_MSG _IOR(IOCTL_LLKD_SED_MAGIC, 4, int)

/* Metadata structure for the 'payload' */
#define MAX_DATA 512

struct sed_ds {
	int data_xform;		   // data transform to apply to the data payload
	int len;			   // length of data payload (bytes)
	int timed_out;		   // 1 if the op timed out
	char shmem[MAX_DATA];  // the payload
};
// Data transformations
enum xform { XF_NONE, XF_RETRIEVE, XF_ENCRYPT, XF_DECRYPT };

#define SHOW_TIME()                               \
	do {                                          \
		pr_debug("%lld ns", ktime_get_real_ns()); \
	} while (0)

#endif /* #ifndef __LLKD_SED_COMMON_H__ */

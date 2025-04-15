#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>

#define DEV_FILE "/dev/ioctl"

#define NEW_MSG "Hello, Linux!!\n"

#define IOCTL_IOC_MAGIC 'd'
#define IOCTL_RESET _IO(IOCTL_IOC_MAGIC, 0)
#define IOCTL_HOWMANY _IOWR(IOCTL_IOC_MAGIC, 1, int)
#define IOCTL_MESSAGE _IOW(IOCTL_IOC_MAGIC, 2, int)

struct ioctl_msg_arg {
    int len;
    char *msg;
};

int main(int argc, char *argv[]) {
    char *msg = NEW_MSG;

    struct ioctl_msg_arg msg_arg = {.len = sizeof(NEW_MSG), .msg = msg};

    int fd = open(DEV_FILE, O_RDONLY);
    int err = ioctl(fd, IOCTL_MESSAGE, &msg_arg);
    printf("retval = %d\n", err);

    return 0;
}

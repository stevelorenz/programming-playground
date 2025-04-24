// Enable this for glibc with feature_test_macros
#define _XOPEN_SOURCE 600

#include <fcntl.h>
#include <stdlib.h>

#include "tlpi_hdr.h"

#include "pty_master_open.h"

int ptyMaterOpen(char *slaveName, size_t snLen) {

    int masterFd = 0;
    int savedErrno = 0;
    char *p = NULL;

    masterFd = posix_openpt(O_RDWR | O_NOCTTY);
    if (-1 == masterFd) {
        return -1;
    }

    if (grantpt(masterFd) == -1) {
        savedErrno = errno;
        close(masterFd); // This call might change errno
        errno = savedErrno;
        return -1;
    }

    if (unlockpt(masterFd) == -1) {
        savedErrno = errno;
        close(masterFd);
        errno = savedErrno;
        return -1;
    }

    p = ptsname(masterFd);
    if (NULL == p) {
        savedErrno = errno;
        close(masterFd);
        errno = savedErrno;
        return -1;
    }

    if (strlen(p) < snLen) {
        strncpy(slaveName, p, snLen);
    } else {
        // Return EOVERFLOW if the buffer size is too small
        close(masterFd);
        errno = EOVERFLOW;
        return -1;
    }

    return 0;
}

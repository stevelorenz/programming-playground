#include <fcntl.h>
#include <sys/ioctl.h>
#include <termios.h>

#include "tlpi_hdr.h"

#include "pty_fork.h"
#include "pty_master_open.h"

#define MAX_SNAME 1000

pid_t ptyFork(int *masterFd, char *slaveName, size_t snLen,
              const struct termios *slaveTermios,
              const struct winsize *slaveWS) {
    int mfd, slaveFd, savedErrno;
    pid_t childPid;
    char slname[MAX_SNAME];

    mfd = ptyMaterOpen(slname, MAX_SNAME);
    if (-1 == mfd) {
        return -1;
    }

    if (NULL != slaveName) {
        if (strlen(slname) < snLen) {
            strncpy(slaveName, slname, snLen);

        } else {
            close(mfd);
            errno = EOVERFLOW;
            return -1;
        }
    }

    childPid = fork();

    if (-1 == childPid) {
        savedErrno = errno;
        close(mfd);
        errno = savedErrno;
        return -1;
    }

    // Parent
    if (childPid != 0) {
        *masterFd = mfd;
        return childPid;
    }

    if (setsid() == -1) {
        err_exit("ptyFork:setid");
    }

    close(mfd);

    slaveFd = open(slname, O_RDWR); // Becomes the controlling tty
    if (-1 == slaveFd) {
        err_exit("ptyFork:open-slave");
    }

    if (NULL != slaveTermios) {
        if (tcsetattr(slaveFd, TCSANOW, slaveTermios) == -1) {
            err_exit("ptyFork: tcsetattr");
        }
    }

    if (slaveWS != NULL) /* Set slave tty window size */
        if (ioctl(slaveFd, TIOCSWINSZ, slaveWS) == -1)
            err_exit("ptyFork:ioctl-TIOCSWINSZ");

    if (dup2(slaveFd, STDIN_FILENO) != STDIN_FILENO) {
        err_exit("ptyFork:dup2-STDIN_FILENO");
    }
    if (dup2(slaveFd, STDOUT_FILENO) != STDOUT_FILENO) {
        err_exit("ptyFork:dup2-STDOUT_FILENO");
    }
    if (dup2(slaveFd, STDERR_FILENO) != STDERR_FILENO) {
        err_exit("ptyFork:dup2-STDERR_FILENO");
    }

    if (slaveFd > STDIN_FILENO) {
        close(slaveFd);
    }

    return 0;
}

#include <fcntl.h>
#include <libgen.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <termios.h>

#include "pty_fork.h"
#include "tlpi_hdr.h"
#include "tty_functions.h"

#define BUF_SIZE 256
#define MAX_SNAME 1000

struct termios ttyOrig;

static void ttyReset(void) {
    if (tcsetattr(STDIN_FILENO, TCSANOW, &ttyOrig) == -1) {
        errExit("tcsetattr");
    }
}

int main(int argc, char *argv[]) {
    char slaveName[MAX_SNAME];
    char *shell;
    int masterFd, scriptFd;
    struct winsize ws;
    fd_set inFds;
    char buf[BUF_SIZE];
    ssize_t numRead;
    pid_t childPid;

    if (tcgetattr(STDIN_FILENO, &ttyOrig) == -1) {
        errExit("tcgetattr");
    }
    if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) < 0) {
        errExit("ioctl-TIOCGWINSZ");
    }

    childPid = ptyFork(&masterFd, slaveName, MAX_SNAME, &ttyOrig, &ws);
    if (-1 == childPid) {
        errExit("ptyFork");
    }

    if (childPid == 0) { /* Child: execute a shell on pty slave */

        /* If the SHELL variable is set, use its value to determine
           the shell execed in child. Otherwise use /bin/sh. */

        shell = getenv("SHELL");
        if (shell == NULL || *shell == '\0')
            shell = "/bin/sh";

        execlp(shell, shell, (char *)NULL);
        errExit("execlp"); /* If we get here, something went wrong */
    }

    // Parent: relay data between terminal and pty master
    scriptFd =
        open((argc > 1) ? argv[1] : "typescript", O_WRONLY | O_CREAT | O_TRUNC,
             S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    if (-1 == scriptFd) {
        errExit("open typescript");
    }

    ttySetRaw(STDIN_FILENO, &ttyOrig);

    if (atexit(ttyReset) != 0) {
        errExit("atexit");
    }

    for (;;) {
        FD_ZERO(&inFds);
        FD_SET(STDIN_FILENO, &inFds);
        FD_SET(masterFd, &inFds);

        if (select(masterFd + 1, &inFds, NULL, NULL, NULL) == -1) {
            errExit("select");
        }

        // stdin -> pty
        if (FD_ISSET(STDIN_FILENO, &inFds)) {
            numRead = read(STDIN_FILENO, buf, BUF_SIZE);
            if (numRead <= 0) {
                exit(EXIT_SUCCESS);
            }
            if (write(masterFd, buf, numRead) != numRead) {
                fatal("partial/failed write (masterFd)");
            }
        }

        // pty -> stdout + file
        if (FD_ISSET(masterFd, &inFds)) {
            numRead = read(masterFd, buf, BUF_SIZE);
            if (numRead <= 0) {
                exit(EXIT_SUCCESS);
            }

            if (write(STDIN_FILENO, buf, numRead) != numRead) {
                fatal("partial/failed write (STDIN_FILENO)");
            }
            if (write(scriptFd, buf, numRead) != numRead) {
                fatal("partial/failed write (scriptFd)");
            }
        }
    }

    exit(EXIT_SUCCESS);
}

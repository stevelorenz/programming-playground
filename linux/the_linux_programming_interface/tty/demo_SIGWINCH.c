#include <signal.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <termios.h>

#include "tlpi_hdr.h"

static void sigwinchHandler(int sig) { printf("Enter sigwinchHandler!\n"); }

int main() {

    struct winsize ws;
    struct sigaction sa;

    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sa.sa_handler = sigwinchHandler;

    if (sigaction(SIGWINCH, &sa, NULL) == -1) {
        errExit("sigaction");
    }

    for (;;) {
        pause();

        if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) == -1) {
            errExit("ioctl");
        }

        printf("Caught SIGWINCH, new window size: %d rows * %d columns\n",
               ws.ws_row, ws.ws_col);
    }

    exit(EXIT_SUCCESS);
}

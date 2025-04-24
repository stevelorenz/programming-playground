#include <termios.h>

#include "tlpi_hdr.h"

#define BUF_SIZE 100

int main(int argc, char *argv[]) {

    struct termios tp, save;
    char buf[BUF_SIZE];

    // Get the current terminal settings and turn echoing off
    if (tcgetattr(STDIN_FILENO, &tp) == -1) {
        errExit("tcgetattr");
    }
    save = tp;
    tp.c_lflag &= ~ECHO;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &tp) == -1) {
        errExit("tcsetattr");
    }

    printf("Enter text: ");
    fflush(stdout);
    if (fgets(buf, BUF_SIZE, stdin) == NULL) {
        printf("Got end-of-file/error on fgets()\n");
    } else {
        printf("\nRead: %s", buf);
    }

    // Restore the orginal terminal settings
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &save) == 1) {
        errExit("tcsetattr");
    }

    exit(EXIT_SUCCESS);
}

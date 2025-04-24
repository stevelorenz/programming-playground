#include <termios.h>
#include <unistd.h>

#include "tty_functions.h"

int ttySetCbreak(int fd, struct termios *prevTermios) {
    struct termios t;

    if (tcgetattr(fd, &t) == -1) {
        return -1;
    }

    if (NULL != prevTermios) {
        *prevTermios = t;
    }

    t.c_lflag &= ~(ICANON | ECHO);
    t.c_lflag |= ISIG;

    t.c_iflag &= ~ICRNL;

    t.c_cc[VMIN] = 1;
    t.c_cc[VTIME] = 0;

    if (tcsetattr(fd, TCSAFLUSH, &t) == -1) {
        return -1;
    }

    return 0;
}

int ttySetRaw(int fd, struct termios *prevTermios) {

    struct termios t;

    if (tcgetattr(fd, &t) == -1) {
        return -1;
    }

    if (NULL != prevTermios) {
        *prevTermios = t;
    }

    t.c_lflag &= ~(ICANON | ISIG | IEXTEN | ECHO);
    t.c_iflag &= ~(BRKINT | ICRNL | IGNBRK | IGNCR | INLCR | INPCK | ISTRIP |
                   IXON | PARMRK);

    t.c_oflag &= ~OPOST;

    t.c_cc[VMIN] = 1;
    t.c_cc[VTIME] = 0;

    if (tcsetattr(fd, TCSAFLUSH, &t) == -1) {
        return -1;
    }

    return 0;
}

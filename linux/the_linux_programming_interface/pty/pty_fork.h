#ifndef FORK_PTY_H
#define FORK_PTY_H

#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>

pid_t ptyFork(int *masterFd, char *slaveName, size_t snLen,
              const struct termios *slaveTermios,
              const struct winsize *slaveWS);

#endif

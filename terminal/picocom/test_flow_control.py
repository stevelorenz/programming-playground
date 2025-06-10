#!/bin/env python3

"""
About: Test the flow control mechanism that can be applied to PTY-based picocom-for-ssh solution

Author: Zuo Xiang (zuoxiang)
"""

import os
import pty
import shlex
import subprocess


def create_pty_pair():
    """"""
    print("- Creata one PTY pair")

    master_fd, slave_fd = pty.openpty()
    master_name = os.ttyname(master_fd)
    slave_name = os.ttyname(slave_fd)
    ret = (master_fd, master_name, slave_fd, slave_name)
    print(
        "- [create_pty_pair] PTY pair created with master: ({}, {}), slave: ({}, {})".format(
            *ret
        )
    )

    print("- [create_pty_pair] chmod 666 of created slave dev")
    subprocess.run(
        shlex.split("sudo chmod 666 {}".format(slave_name)),
        capture_output=True,
        text=True,
        check=True,
    )

    return ret


def main():
    print("# Test flow control")

    pty = create_pty_pair()

    with open(pty[-1]) as device_file:
        result = subprocess.run(
            shlex.split("sudo stty -a"),
            stdin=device_file,
            capture_output=True,
            text=True,
            check=True,
        )

    print("- [main] Display all the current settings of PTY device {}".format(pty[-1]))
    print(result.stdout)


if __name__ == "__main__":
    main()

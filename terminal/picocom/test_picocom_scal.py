#!/bin/env python3

"""
About: Test the scalability of picocom using pseudo terminal devices
Author: Zuo Xiang (zuoxiang)
"""

import argparse
import os
import pty
import subprocess
import sys
import time


def run_cleanup(ptys):
    """

    :param ptys:
    :type ptys:
    """
    for pty in ptys:
        os.close(pty["master_fd"])
        os.close(pty["slave_fd"])


def create_pty_pairs(num):
    """Create PTY device pairs for testing

    :param num:
    :type num:
    :return: A list of created PTY pairs
    :rtype: list
    """
    print("# Create {} PTY pairs".format(num))
    ptys = list()

    for n in range(num):
        master_fd, slave_fd = pty.openpty()
        master_name = os.ttyname(master_fd)
        slave_name = os.ttyname(slave_fd)
        print(
            "- [{}] PTY pair created with master: ({}, {}), slave: ({}, {})".format(
                n, master_fd, master_name, slave_fd, slave_name
            )
        )
        ptys.append(
            {
                "master_fd": master_fd,
                "slave_fd": slave_fd,
                "slave_name": slave_name,
            }
        )
        if n == num - 1:
            print("NOTE: The last PTY pair is for interactive testing and debugging")

    return ptys


def run_picocom(ptys):
    picocoms = list()
    for n, pty in enumerate(ptys):
        print(pty)
        process = subprocess.Popen(
            ["picocom", pty["slave_name"]],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        picocoms.append(process)
        print(
            "- [{}] Start picocom process with pid: {} on slave device: {}".format(
                n, process.pid, pty["slave_name"]
            )
        )


def benchmark():
    """TODO"""
    pass


def main():
    """Main function"""

    parser = argparse.ArgumentParser(description="Test picocom scalability")

    parser.add_argument(
        "--num",
        type=int,
        default=1,
        help="Number of picocom processes",
    )

    args = parser.parse_args()

    print("# Create {} PTY pairs and run {} picocom processes.")
    print("  - Create one more PTY pair just for interactive tesing")
    ptys = create_pty_pairs(args.num + 1)
    # Pop the last PTY pair which is used for interactive testing
    pty_inter = ptys.pop()
    run_picocom(ptys)

    try:
        while True:
            time.sleep(1)
            # TODO: Create a function to send data to picocom with different data rate
            os.write(
                pty_inter["master_fd"], "Hello from master side!\n".encode("utf-8")
            )
    except KeyboardInterrupt:
        print("\nKeyboardInterrupt detected! Run cleanups")
        run_cleanup(ptys)
        sys.exit(0)


if __name__ == "__main__":
    main()

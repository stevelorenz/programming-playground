#!/bin/env python3

"""
About: Test the scalability of picocom using pseudo terminal devices
Author: Zuo Xiang (zuoxiang)
"""

import argparse
import os
import pty
import signal
import subprocess
import time
import random
import string

TEST_DATA_BUF_SIZE = 1024


def term_processes(processes):
    """Terminate all given processes using SIGTERM

    :param processes:
    :type processes:
    """
    for process in processes:
        process.send_signal(signal.SIGTERM)
        process.wait(timeout=10)


def run_cleanup(ptys, picocoms, log_files):
    """Run cleanup of all created resources

    :param ptys:
    :type ptys:
    """
    print("# Enter run_cleanup")
    for pty in ptys:
        os.close(pty["master_fd"])
        os.close(pty["slave_fd"])

    for f in log_files:
        f.close()

    term_processes(picocoms)


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
            "- [create_pty_pairs] {} PTY pair created with master: ({}, {}), slave: ({}, {})".format(
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
    log_files = list()
    for n, pty in enumerate(ptys):
        output_file_path = "./{}_{}_output.log".format(
            pty["master_fd"], pty["slave_fd"]
        )
        output_file = open(output_file_path, "w")
        process = subprocess.Popen(
            ["picocom", pty["slave_name"]],
            stdin=subprocess.PIPE,
            stdout=output_file,
            stderr=subprocess.STDOUT,
            preexec_fn=os.setpgrp,  # Use a new process group to avoid signal trapping
        )
        picocoms.append(process)
        log_files.append(output_file)
        print(
            "- [run_picocom] {} Start picocom process with pid: {} on slave device: {}".format(
                n, process.pid, pty["slave_name"]
            )
        )

    return picocoms, log_files


def generate_random_string(length):
    characters = string.ascii_letters + string.digits
    random_string = "".join(random.choices(characters, k=length))
    return random_string


def run_workload_traffic(ptys):
    """TODO: Take care of OpenSSH processes here!!!

    :param ptys:
    :type ptys:
    """
    # TODO: Send with the theoretical maximal baudrate of a real TTY device
    for pty in ptys:
        input_fd = pty["master_fd"]
        test_data = generate_random_string(TEST_DATA_BUF_SIZE)
        os.write(input_fd, test_data.encode())


def get_cpu_memory_usage(pid):
    """Get CPU and memory usage of the given PID (ONLY for Linux)

    :param pid:
    :type pid:
    :return:
    :rtype:
    """
    try:
        stat_path = "/proc/{}/stat".format(pid)
        status_path = "/proc/{}/status".format(pid)

        with open(stat_path, "r") as stat_file:
            stat_fields = stat_file.readline().split()
            # CPU usage is the sum of utime (14th) and stime (15th) fields in jiffies
            cpu_utime = int(stat_fields[13])  # User mode CPU time (ticks)
            cpu_stime = int(stat_fields[14])  # Kernel mode CPU time (ticks)
            cpu_total_time = cpu_utime + cpu_stime

        with open(status_path, "r") as status_file:
            memory_rss = None
            for line in status_file:
                if line.startswith("VmRSS:"):
                    # VmRSS (Resident Set Size) gives the memory usage in KB
                    memory_rss_kb = int(line.split()[1])  # Extract value in KB
                    memory_rss = memory_rss_kb * 1024  # Convert to bytes (B)
                    break

        return cpu_total_time, memory_rss

    except FileNotFoundError:
        # Process might have terminated
        return None, None


def printBenchmarkResult(bm_data):
    """Print the CPU and memory usage in a table

    :param bm_data:
    :type bm_data:
    """
    rows = []
    total_avg_cpu = 0
    total_avg_memory = 0

    for process_id, measurements in bm_data.items():
        total_cpu = 0
        total_memory = 0
        max_cpu = float("-inf")  # Initialize max values
        max_memory = float("-inf")

        for cpu_time, memory in measurements:
            total_cpu += cpu_time
            total_memory += memory
            max_cpu = max(max_cpu, cpu_time)
            max_memory = max(max_memory, memory)

        avg_cpu_time = total_cpu / len(measurements)
        avg_memory = total_memory / len(measurements)

        total_avg_cpu += avg_cpu_time
        total_avg_memory += avg_memory

        rows.append(
            (
                process_id,
                "picocom",
                "{:.2f}".format(avg_cpu_time),
                max_cpu,
                "{:.2f}".format(avg_memory),
                max_memory,
            )
        )

    rows.append(
        (
            "Summary",
            "-",
            "{:.2f}".format(total_avg_cpu),
            "-",
            "{:.2f}".format(total_avg_memory),
            "-",
        )
    )

    # Create a table header
    header = "{:<12}{:<12}{:<20}{:<15}{:<20}{:<15}".format(
        "Process ID",
        "Type",
        "Average CPU Time",
        "Max CPU Time",
        "Average Memory (B)",
        "Max Memory (B)",
    )
    separator = "-" * len(header)

    # Print the table to the console
    print(header)
    print(separator)
    for row in rows:
        print(
            "{:<12}{:<12}{:<20}{:<15}{:<20}{:<15}".format(
                row[0], row[1], row[2], row[3], row[4], row[5]
            )
        )


def main():
    """Main function"""

    parser = argparse.ArgumentParser(description="Test picocom scalability")

    parser.add_argument(
        "--num",
        type=int,
        default=1,
        help="Number of picocom processes",
    )
    parser.add_argument(
        "--duration",
        type=int,
        default=5,
        help="Test duration in seconds",
    )

    args = parser.parse_args()

    print(
        "# Create {} PTY pairs and run {} picocom processes.".format(
            args.num + 1, args.num
        )
    )
    print("  - Create one more PTY pair just for interactive tesing")
    ptys = create_pty_pairs(args.num + 1)
    # Pop the last PTY pair which is used for interactive testing
    _ = ptys.pop()
    picocoms, log_files = run_picocom(ptys)
    time.sleep(1)
    run_workload_traffic(ptys)

    bm_data = {}
    for process in picocoms:
        bm_data[process.pid] = list()

    try:
        print("\n" * 3, end="")
        for d in range(args.duration):
            # Run CPU and memory benchmarking
            for process in picocoms:
                bm_data[process.pid].append(get_cpu_memory_usage(process.pid))
            print("- Current duration: {} s".format(d + 1))
            time.sleep(1)
        print("\n" * 3, end="")
        printBenchmarkResult(bm_data)
        print("\n" * 3, end="")
    except KeyboardInterrupt:
        print("\nKeyboardInterrupt detected! Run cleanups")
    finally:
        run_cleanup(ptys, picocoms, log_files)


if __name__ == "__main__":
    main()

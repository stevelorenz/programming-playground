#!/bin/env python3

"""
About: Test the scalability of picocom using pseudo terminal devices

       - Since it's difficult to setup a lot of real physical terminal devices for testing purposes, pseudo terminal
         devices are used here for quick performance estimation

Author: Zuo Xiang (zuoxiang)

TODO:
"""

import argparse
import os
import pty
import random
import signal
import string
import subprocess
import sys
import threading
import time

#
# Constants
#
TEST_DATA_BUF_SIZE = 1024

#
# Global variables
#
workload_traffic_stop_event = threading.Event()


#
# Helper Functions
#
def eprint(*args, **kwargs):
    """Error print to stderr"""
    print(*args, file=sys.stderr, **kwargs)


def term_processes(processes, signal=signal.SIGTERM, wait_time_out=10):
    """Try to terminate all given processes gracefully

    :param processes:
    :type processes:
    :param signal:
    :type signal:
    :param wait_time_out:
    :type wait_time_out:
    """
    for process in processes:
        process.send_signal(signal)
        process.wait(wait_time_out)


def run_cleanup(ptys, picocoms, output_log_files):
    """Try to run cleanup of all created resources

    :param ptys:
    :type ptys:
    :param picocoms:
    :type picocoms:
    :param log_files:
    :type log_files:
    """
    print("# Enter run_cleanup")

    for pty in ptys:
        os.close(pty["master_fd"])
        os.close(pty["slave_fd"])

    for f in output_log_files:
        f.close()

    term_processes(picocoms)


def create_pty_pairs(num):
    """Create PTY device pairs for testing

    :param num: Number of to-be-created PTY pairs
    :type num: int
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
    """Run picocom processes which are listening on the given PTY slave devices

    :param ptys:
    :type ptys:
    :return:
    :rtype:
    """
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
    """Generate random string with a given length
       Just for picocom workload testing

    :param length:
    :type length:
    :return:
    :rtype:
    """
    characters = string.ascii_letters + string.digits
    random_string = "".join(random.choices(characters, k=length))
    return random_string


def run_workload_traffic(ptys, baud_rate=115200):
    """Run workload traffic to test the performance of picocoms

    :param ptys:
    :type ptys:
    :return:
    :rtype:
    """
    bits_per_byte = 10  # 8 data bits + 1 start bit + 1 stop bit
    sleep_time = (TEST_DATA_BUF_SIZE * bits_per_byte) / baud_rate

    input_data = dict()
    for pty in ptys:
        input_data[pty["master_fd"]] = list()

    while not workload_traffic_stop_event.is_set():
        for pty in ptys:
            test_data = generate_random_string(TEST_DATA_BUF_SIZE)
            os.write(pty["master_fd"], test_data.encode())
            input_data[pty["master_fd"]].append(test_data)
        time.sleep(sleep_time)

    # Dump workload traffic (input traffic) into files
    for pty in ptys:
        with open(
            "./{}_{}_input.log".format(pty["master_fd"], pty["slave_fd"]), "w+"
        ) as file:
            for data in input_data[pty["master_fd"]]:
                file.write(data)


def check_workload_traffic_result(ptys):
    print("# Check the input and output result of test workload traffic")
    for pty in ptys:
        input_log_file_name = "{}_{}_input.log".format(
            pty["master_fd"], pty["slave_fd"]
        )
        output_log_file_name = "{}_{}_output.log".format(
            pty["master_fd"], pty["slave_fd"]
        )

        with open(input_log_file_name, "r") as file:
            input_data = file.read()

        with open(output_log_file_name, "r") as file:
            outputs = file.readlines()
            index = outputs.index("Terminal ready\n")
            output_data = outputs[index + 1]

        if input_data != output_data:
            print(
                "- [check_workload_traffic_result] FAIL! Diff detected when comparing {} and {}".format(
                    input_log_file_name, output_log_file_name
                )
            )
        else:
            print(
                "- [check_workload_traffic_result] PASS! when comparing {} and {}".format(
                    input_log_file_name, output_log_file_name
                )
            )


def get_cpu_time(pid):
    """Retrieve the total (accumulated) CPU time (user + system) of a process in seconds.
    TODO: Check if this method works on real hardware boxes...

    :param pid:
    :type pid:
    :return:
    :rtype:
    """
    SC_CLK_TCK = os.sysconf(
        os.sysconf_names["SC_CLK_TCK"]
    )  # Get clock ticks per second
    stat_path = f"/proc/{pid}/stat"
    with open(stat_path, "r") as stat_file:
        stat_fields = stat_file.readline().split()
        cpu_utime = int(stat_fields[13])  # User mode CPU time (ticks)
        cpu_stime = int(stat_fields[14])  # Kernel mode CPU time (ticks)
        return (cpu_utime + cpu_stime) / SC_CLK_TCK  # Convert ticks to seconds


def get_cpu_memory_usage(pid, cpu_monitor_interval=0.001):
    """Get CPU and memory usage of the given PID (ONLY for Linux)

       - For CPU: Use utime and stime fields and calculate the percentage with a given interval
       - For memory: Use the VmRSS field

    :param pid:
    :type pid:
    :param cpu_monitor_interval:
    :type cpu_monitor_interval:
    :return:
    :rtype:
    """
    try:
        status_path = "/proc/{}/status".format(pid)
        with open(status_path, "r") as status_file:
            memory_usage = None
            for line in status_file:
                if line.startswith("VmRSS:"):
                    # VmRSS (Resident Set Size) gives the memory usage in KB
                    memory_rss_kb = int(line.split()[1])  # Extract value in KB
                    memory_usage = memory_rss_kb * 1024  # Convert to bytes (B)
                    break

        cpu_time_start = get_cpu_time(pid)
        time.sleep(cpu_monitor_interval)  # Wait for the specified interval
        cpu_time_end = get_cpu_time(pid)
        # Try to get the CPU usage in the unit of percentage
        cpu_usage = ((cpu_time_end - cpu_time_start) / cpu_monitor_interval) * 100
        cpu_usage = round(cpu_usage, 2)

        return cpu_usage, memory_usage

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
        "Average CPU Usage (%)",
        "Max CPU Usage (%)",
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
    """Main function. LUGTSD"""

    parser = argparse.ArgumentParser(
        description="(Try to ;)) Test picocom+OpenSSH combo scalability"
    )

    parser.add_argument(
        "-n",
        "--num",
        type=int,
        default=3,
        help="Number of picocom processes",
    )
    parser.add_argument(
        "-d",
        "--duration",
        type=int,
        default=10,
        help="Test duration in seconds.",
    )

    args = parser.parse_args()
    if args.num <= 0:
        eprint("Error: Number of picocom processes must be positive!")
        sys.exit(1)
    if args.duration <= 0:
        eprint("Error: Test duration must be positive!")
        sys.exit(1)

    print(
        "# Create {} PTY pairs and run {} picocom processes.".format(
            args.num + 1, args.num
        )
    )
    print("  - Create one more PTY pair just for interactive tesing")
    ptys = create_pty_pairs(args.num + 1)
    # Pop the last PTY pair which is used for interactive testing
    _ = ptys.pop()
    picocoms, output_log_files = run_picocom(ptys)
    time.sleep(1)

    print(
        "# Start the workload_traffic_thread to inject test traffic for created PTY pairs"
    )
    workload_traffic_thread = threading.Thread(
        target=run_workload_traffic, args=(ptys,), daemon=True
    )
    workload_traffic_thread.start()

    bm_data = {}
    for process in picocoms:
        bm_data[process.pid] = list()

    try:
        print("\n" * 3, end="")
        for d in range(1, args.duration + 1, 1):
            # Run CPU and memory benchmarking
            for process in picocoms:
                bm_data[process.pid].append(get_cpu_memory_usage(process.pid))
            print("- Current duration: {} s".format(d))
            time.sleep(1)
        print("\n" * 3, end="")
        printBenchmarkResult(bm_data)
        print("\n" * 3, end="")

        workload_traffic_stop_event.set()
        workload_traffic_thread.join()

        check_workload_traffic_result(ptys)

    except KeyboardInterrupt:
        print("\nKeyboardInterrupt detected! Run cleanups")
    finally:
        run_cleanup(ptys, picocoms, output_log_files)


if __name__ == "__main__":
    main()

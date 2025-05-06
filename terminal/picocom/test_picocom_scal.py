#!/bin/env python3

"""
About: Test the scalability of picocom using pseudo terminal devices

       - Since it's difficult to setup a lot of real physical terminal devices for testing purposes, pseudo terminal
         devices are used here for quick performance estimation

Author: Zuo Xiang (zuoxiang)

TODO:
    - Learn and add the openSSH part!
"""

import argparse
import os
import pty
import random
import re
import signal
import string
import subprocess
import sys
import threading
import time

#
# Constants
#
# Data buff/block size used by workload traffic
TEST_DATA_BUF_SIZE = 1024

#
# Global variables
#
# Event to stop the sending of workload traffic in the worker thread
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


def run_cleanup(ptys, picocoms, output_log_files, sshs):
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
    term_processes(sshs)


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


def get_ssh_server_pid(client_pid):
    """Get the corresponded sshd server-side PID with the given client_pid.
       This assuming that both client and server SSH processes are running on the same machine. Namely, localhost is
       used here just for basic function and performance testing

    :param client_pid:
    :type client_pid:
    :raises Exception:
    :return:
    :rtype:
    """
    try:
        # Use 'lsof' to get the network connection details of the client process
        lsof_command = "lsof -i -n -P | grep ssh | grep {}".format(client_pid)
        lsof_output = subprocess.check_output(lsof_command, shell=True, text=True)

        # Parse the output to extract the local port
        # Example output: ssh     12345 user   3u  IPv4  1234567      0t0  TCP 127.0.0.1:port->127.0.0.1:22 (ESTABLISHED)
        match = re.search(r"TCP \d+\.\d+\.\d+\.\d+:(\d+)->", lsof_output)
        if not match:
            raise RuntimeError("Could not parse the local port from lsof output.")

        local_port = match.group(1)

        # Use 'lsof' again to find the server-side process listening on this port
        lsof_server_command = "sudo lsof -i -n -P | grep sshd | grep {}".format(
            local_port
        )
        server_output = subprocess.check_output(
            lsof_server_command, shell=True, text=True
        )

        for line in server_output.splitlines():
            columns = line.split()
            if len(columns) > 2 and columns[2] != "root":  # Check the user column
                server_pid = int(columns[1])  # PID is the second column
                return server_pid
        raise RuntimeError("Can NOT find the SSH server PID in lsof output")

    except subprocess.CalledProcessError as e:
        raise RuntimeError("Error running lsof command: {}".format(e))
    except Exception as e:
        raise RuntimeError("Error retrieving SSH server PID: {}".format(e))


def run_ssh_clients(num, username, port):
    """Run SSH clients connecting to localhost for performance benchmarking

    :param num:
    :type num:
    :param username:
    :type username:
    :param port:
    :type port:
    :return:
    :rtype:
    """
    ssh_command = ["ssh", "{}@127.0.0.1".format(username), "-p", "{}".format(port)]
    sshs = list()

    try:
        for n in range(num):
            process = subprocess.Popen(
                ssh_command,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            # Give the process sometime to init... Otherwise, error is raised when trying to retrieve server-side PID
            time.sleep(1)
            client_pid = process.pid
            server_pid = get_ssh_server_pid(client_pid)
            sshs.append((process, client_pid, server_pid))
            print(
                "- [run_ssh_clients] {} Start ssh process on port: {} with PID: {}, server-side PID: {}".format(
                    n, port, client_pid, server_pid
                )
            )

    except Exception as e:
        print("SSH localhost error occurred: {}".format(e))
        raise e

    return sshs


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
    all_pass = True
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
            all_pass = False

    if all_pass:
        print(
            "- [check_workload_traffic_result] All tests have been passed! Congratulations! Man!"
        )
    else:
        print(
            "- [check_workload_traffic_result] Test failure detected! Please check log files!"
        )
    print("\n" * 3, end="")


def get_process_cpu_usage_with_top(pid):
    """Get the CPU and memory usage of a specific process using the top command.
    WARNING: Comparing to directly using proc, this top based approach could take a lot of time and impact the expected
    benchmarking duration... So, be careful when using this method...

    :param pid: Process ID
    :type pid:
    :raises ValueError:
    :raises RuntimeError:
    :return:
    :rtype:
    """
    try:
        # Run the `top` command for the specific PID
        result = subprocess.run(
            ["top", "-bn1", "-p", str(pid)], capture_output=True, text=True, check=True
        )
        # Parse the output to find the line corresponding to the PID
        for line in result.stdout.splitlines():
            if line.strip().startswith(str(pid)):
                # The line for the process includes columns for CPU and memory usage
                parts = line.split()
                # The columns typically follow this format:
                # PID USER  PR  NI  VIRT  RES  SHR  S  %CPU  %MEM  TIME+  COMMAND
                cpu_usage = float(parts[8])  # %CPU column
                return cpu_usage

        raise ValueError(
            "Could not find usage information for PID {} in top output.".format(pid)
        )
    except Exception as e:
        raise RuntimeError("Failed to retrieve process usage: {}".format(e))


def get_system_cpu_usage_with_top():
    """Get the whole system CPU usage percentage using the top command."""
    try:
        # Run the `top` command in batch mode to capture output
        result = subprocess.run(
            ["top", "-bn1"], capture_output=True, text=True, check=True
        )

        # Parse the output to find the line containing "Cpu(s)"
        for line in result.stdout.splitlines():
            if "Cpu(s)" in line:
                # Extract the idle percentage from the line
                parts = line.split(",")
                idle_part = [part for part in parts if "id" in part][
                    0
                ]  # Find the 'id' section
                idle_percentage = float(
                    idle_part.split()[0]
                )  # Extract the numeric value

                # CPU usage is calculated as (100 - idle)
                cpu_usage = 100 - idle_percentage
                return cpu_usage
        raise ValueError("Could not find CPU usage information in top output.")
    except Exception as e:
        raise RuntimeError("Failed to retrieve CPU usage: {}".format(e))


def get_process_memory_usage_with_proc(pid):
    """Get memory usage of the given PID (ONLY for Linux)

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

        return memory_usage

    except Exception as e:
        raise RuntimeError("Failed to get memory usage using proc: {}".format(e))


def printBenchmarkResult(bm_data, bm_pid_cpu):
    """Print the CPU and memory usage in a table
    TODO: Refactor this to make it more elegant...

    :param bm_data:
    :type bm_data:
    :param bm_pid_cpu:
    :type bm_pid_cpu
    """
    rows = list()
    total_avg_cpu = 0
    total_avg_memory = 0

    for process_id, data_entry in bm_data.items():
        total_cpu = 0
        total_memory = 0
        max_cpu = float("-inf")  # Initialize max values
        max_memory = float("-inf")

        for cpu, memory in data_entry[1]:
            total_cpu += cpu
            total_memory += memory
            max_cpu = max(max_cpu, cpu)
            max_memory = max(max_memory, memory)

        avg_cpu = total_cpu / len(data_entry[1])
        avg_memory = total_memory / len(data_entry[1])

        total_avg_cpu += avg_cpu
        total_avg_memory += avg_memory

        if not bm_pid_cpu:
            avg_cpu_item = "NA"
            max_cpu_item = "NA"
        else:
            avg_cpu_item = "{:.2f}".format(avg_cpu)
            max_cpu_item = "{:.2f}".format(max_cpu)
        rows.append(
            (
                process_id,
                "{}".format(data_entry[0]),
                avg_cpu_item,
                max_cpu_item,
                "{:.2f}".format(avg_memory),
                max_memory,
            )
        )

    if not bm_pid_cpu:
        total_cpu_item = "NA"
    else:
        total_cpu_item = "{:.2f}".format(total_avg_cpu)
    rows.append(
        (
            "Summary",
            "-",
            total_cpu_item,
            "-",
            "{:.2f}".format(total_avg_memory),
            "-",
        )
    )

    # Create a table header
    if not bm_pid_cpu:
        header = "{:<15}{:<15}{:<25}{:<25}".format(
            "Process ID",
            "Type",
            "Average Memory (B)",
            "Max Memory (B)",
        )
    else:
        header = "{:<15}{:<15}{:<25}{:<25}{:<25}{:<25}".format(
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
        if not bm_pid_cpu:
            print("{:<15}{:<15}{:<25}{:<25}".format(row[0], row[1], row[4], row[5]))
        else:
            print(
                "{:<15}{:<15}{:<25}{:<25}{:<25}{:<25}".format(
                    row[0], row[1], row[2], row[3], row[4], row[5]
                )
            )


def main():
    """Main function. LUGTSD"""

    parser = argparse.ArgumentParser(
        description="(Try to ;)) Test picocom+OpenSSH combo scalability using emulation"
    )

    parser.add_argument(
        "-n",
        "--num",
        type=int,
        default=5,
        help="Number of picocom processes",
    )
    parser.add_argument(
        "-d",
        "--duration",
        type=int,
        default=5,
        help="(Expected) Test duration in seconds. The actual duration could be longer due to the benchmarking time cost",
    )
    parser.add_argument(
        "--bm_pid_cpu",
        default=False,
        action="store_true",
        help="Enable benchmarking CPU usage of each involved process using PID and top (time cost is intensive)",
    )
    parser.add_argument(
        "--ssh_username",
        type=str,
        default="vagrant",
        help="SSH username used for OpenSSH tests. Instead of password, key based authentication is used!",
    )
    parser.add_argument(
        "--ssh_port",
        type=int,
        default=22,
        help="SSH port",
    )

    args = parser.parse_args()
    if args.num <= 0:
        eprint("Error: Number of picocom processes must be positive!")
        sys.exit(1)
    if args.duration <= 0:
        eprint("Error: Test duration must be positive!")
        sys.exit(1)
    if args.ssh_port <= 0:
        eprint("Error: SSH port must be positive!")
        sys.exit(1)

    # System-level CPU usages
    system_cpu_usages = list()
    system_cpu_usage_before = (
        get_system_cpu_usage_with_top()
    )  # "IDLE" CPU usage before running any tests

    print(
        "# Run {} SSH connections on localhost for performance benchmarking. Username: {}, port: {}".format(
            args.num, args.ssh_username, args.ssh_port
        )
    )
    sshs = run_ssh_clients(args.num, args.ssh_username, args.ssh_port)

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

    # Set daemon to True, so the worker thread will terminate when main thread ends
    workload_traffic_thread = threading.Thread(
        target=run_workload_traffic, args=(ptys,), daemon=True
    )
    workload_traffic_thread.start()

    bm_data = {}
    bm_pids = list()
    # Add both picocom and SSH processes into the benchmarking list
    for process in picocoms:
        bm_data[process.pid] = ("picocom", list())
        bm_pids.append(process.pid)
    for ssh in sshs:
        bm_data[ssh[2]] = ("sshd", list())
        bm_pids.append(ssh[2])

    try:
        print("\n" * 3, end="")
        if args.bm_pid_cpu:
            print(
                "- Enable benchmarking CPU usage of each involved process using PID and top"
            )
        else:
            print(
                "- Benchmarking CPU usage of each involved process is disabled! Set usage to -1 just for NA!"
            )

        test_start = time.time()
        # Start the actual test and benchmarking iterations
        for i in range(1, args.duration + 1, 1):
            # Try to run per-process CPU and memory benchmarking
            bm_start = time.time()
            # WARNING: The benchmarking needs some time... So, the actual time spent on benchmarking measurements can be
            # longer or even much longer than one second, which is the designed monitoring step length...
            for pid in bm_pids:
                if args.bm_pid_cpu:
                    entry = (
                        get_process_cpu_usage_with_top(pid),
                        get_process_memory_usage_with_proc(pid),
                    )
                else:
                    entry = (
                        -1,
                        get_process_memory_usage_with_proc(pid),
                    )
                bm_data[pid][1].append(entry)
            bm_duration = time.time() - bm_start
            print(
                "- Current iteration index:  {}, time taken for benchmarking: {:.2f}".format(
                    i, bm_duration
                )
            )
            # Run system-level CPU measurements
            system_cpu_usage_after = get_system_cpu_usage_with_top()
            system_cpu_usage_diff = system_cpu_usage_after - system_cpu_usage_before
            system_cpu_usages.append(system_cpu_usage_diff)

            time.sleep(max(0, 1 - bm_duration))

        print("\n" * 3, end="")
        print("=" * 200)
        printBenchmarkResult(bm_data, args.bm_pid_cpu)
        print("\n" * 3, end="")

        system_cpu_usages = [
            x for x in system_cpu_usages if x >= 0
        ]  # Filter out negative/none-sense values...
        print(
            "# Whole system CPU usage: before: {:.2f}%, average: {:.2f}%, max: {:.2f}%".format(
                system_cpu_usage_before,
                sum(system_cpu_usages) / len(system_cpu_usages),
                max(system_cpu_usages),
            )
        )
        print("=" * 200)
        print("\n" * 3, end="")
        workload_traffic_stop_event.set()
        workload_traffic_thread.join()  # Wait until the work thread terminates

        check_workload_traffic_result(ptys)

        test_duration = time.time() - test_start
        print(
            "# Expected test duration: {:.2f} seconds, actual duration: {:.2f} seconds".format(
                args.duration, test_duration
            )
        )

    except KeyboardInterrupt:
        print("\nKeyboardInterrupt detected! Run cleanups")
    finally:
        run_cleanup(ptys, picocoms, output_log_files, [s[0] for s in sshs])


if __name__ == "__main__":
    main()

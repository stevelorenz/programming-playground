#!/bin/bash

module="scull"
device="scull"
mode="666"
group=0

function load() {
	insmod ./$module.ko $* || exit 1

	rm -f /dev/${device}[0-2]

	major=$(awk -v device="$device" '$2==device {print $1}' /proc/devices)
	mknod /dev/${device}0 c $major 0
	mknod /dev/${device}1 c $major 1
	mknod /dev/${device}2 c $major 2

	chgrp $group /dev/$device[0-2]
	chmod $mode /dev/$device[0-2]
}

function unload() {
	rm -f /dev/${device}[0-2]
	rmmod $module || exit 1
}

arg=${1:-"load"}
case $arg in
load)
	echo "# Load the scull kernel module"
	load
	;;
unload)
	echo "# Unload the scull kernel module"
	unload
	;;
reload)
	echo "# Reload the scull kernel module"
	(unload)
	load
	;;
*)
	echo "Usage: $0 {load | unload | reload}"
	echo "Default is load"
	exit 1
	;;
esac

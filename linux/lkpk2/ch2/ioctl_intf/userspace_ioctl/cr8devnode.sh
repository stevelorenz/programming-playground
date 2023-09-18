#!/bin/bash

name=$(basename "$0")
OURMODNAME="ioctl_llkd_kdrv"
DEVNM=/dev/ioctl_intf

MAJOR=$(grep "${OURMODNAME}" /proc/devices | awk '{print $1}')
[ -z "${MAJOR}" ] && {
	echo "${name}: failed to retreive the major #, aborting ...
  (is the ${OURMODNAME} device driver loaded ?)"
	exit 1
}
echo "major number is ${MAJOR}"
sudo rm -f "${DEVNM}" # rm any stale instance
sudo mknod "${DEVNM}" c "${MAJOR}" 0
ls -l "${DEVNM}"
exit 0

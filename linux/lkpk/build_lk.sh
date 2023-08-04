#!/bin/bash
#
# About: Build Linux kernel from source for a Ubuntu server
#

if [[ -z "${LK_SRC}" ]]; then
	echo "Error: Environment variable LK_SRC is NOT defined!"
	echo "LK_SRC MUST be the directory of Linux kernel source code!"
	exit 1
fi

echo "* Linux kernel source directory: ${LK_SRC}"

ORIGINAL_DIR=$(pwd)

cd "${LK_SRC}" || exit 1

make mrproper
make clean
make distclean

# Copy the config file of current installed Linux
cp -v "/boot/config-$(uname -r)" .config
# Check loaded kernel modules and update .config
make localmodconfig

# Disable security related signings (JUST FOR LOCAL DEV AND LEARNING!)
scripts/config --disable SYSTEM_TRUSTED_KEYS
scripts/config --disable SYSTEM_REVOCATION_KEYS
scripts/config --disable CONFIG_MODULE_SIG
scripts/config --set-str CONFIG_SYSTEM_TRUSTED_KEYS ""
scripts/config --set-str CONFIG_SYSTEM_REVOCATION_KEYS ""

fakeroot make -j8 || cd "${ORIGINAL_DIR}" || exit 1

#!/bin/bash
#
# About: Build a DEV/Learn Linux kernel from source code for a Ubuntu server
#        - Tested on Ubuntu Server 22.04
#

if [[ -z "${LK_SRC}" ]]; then
	echo "Error: Environment variable LK_SRC is NOT defined!"
	echo "LK_SRC MUST be the directory of Linux kernel source code!"
	exit 1
fi

echo "* Linux kernel source directory: ${LK_SRC}"

cd "${LK_SRC}" || exit 1

# Clean "everything"
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

# Enable debugging features
scripts/config --enable CONFIG_DEBUG_INFO
scripts/config --enable CONFIG_DEBUG_FS
scripts/config --enable CONFIG_MAGIC_SYSRQ
scripts/config --enable CONFIG_DEBUG_KERNEL
scripts/config --enable CONFIG_DEBUG_MISC
scripts/config --enable CONFIG_SLUB_DEBUG
scripts/config --enable CONFIG_MEMORY_INIT
scripts/config --enable CONFIG_KASAN
scripts/config --enable CONFIG_DEBUG_SHIRQ
scripts/config --enable CONFIG_DEBUG_STACK_END_CHECK
scripts/config --enable CONFIG_PROVE_LOCKING
scripts/config --enable CONFIG_LOCK_STAT
scripts/config --enable CONFIG_ATOMIC_SLEEP
scripts/config --enable CONFIG_STACKTRACE
scripts/config --enable CONFIG_FTRACE
scripts/config --enable CONFIG_DEBUG_ON_DATA_CORRUPTION
scripts/config --enable CONFIG_KGDB
scripts/config --enable CONFIG_UBSAN
scripts/config --enable CONFIG_EARLY_PRINTK
scripts/config --enable CONFIG_DEBUG_BOOT_PARAMS

# Run build with fakeroot
fakeroot make -j"$(nproc)"

#!/bin/bash
#
# About: Install all required Ubuntu/Debian packages for melp3
#

# Check if the script is run with sudo
if [ "$(id -u)" -eq 0 ]; then
    echo "ERROR: This script should not be run as root or with sudo."
    echo "Please run the script without using sudo."
    exit 1
fi

echo "- Install required packages for melp3"
sudo apt-get update
sudo apt-get install -y \
    autoconf \
    automake \
    chrpath \
    device-tree-compiler \
    diffstat \
    e2fsprogs \
    g++ \
    gcc \
    gcc-arm-linux-gnueabi \
    genext2fs \
    git \
    libtool \
    libtool-bin \
    m4 \
    make \
    make \
    mtd-utils \
    patch \
    pkg-config \
    qemu-system \
    squashfs-tools \
    u-boot-tools \
    util-linux \


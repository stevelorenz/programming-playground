#!/bin/bash

PATH=${HOME}/x-tools/arm-unknown-linux-gnueabi/bin/:$PATH

make ARCH=arm CROSS_COMPILE=arm-unknown-linux-gnueabi- mrproper
if [ $? != 0 ]; then
    echo "ERROR: mrproper"
    exit
fi

make ARCH=arm versatile_defconfig
if [ $? != 0 ]; then
    echo "ERROR: versatile_defconfig"
    exit
fi

make -j4 ARCH=arm CROSS_COMPILE=arm-unknown-linux-gnueabi- zImage
if [ $? != 0 ]; then
    echo "ERROR: zImage"
    exit
fi

make -j4 ARCH=arm CROSS_COMPILE=arm-unknown-linux-gnueabi- modules
if [ $? != 0 ]; then
    echo "ERROR: modules"
    exit
fi

make ARCH=arm CROSS_COMPILE=arm-unknown-linux-gnueabi- dtbs
if [ $? != 0 ]; then
    echo "ERROR: dtbs"
    exit
fi

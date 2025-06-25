#!/bin/bash

PATH=${HOME}/x-tools/arm-cortex_a8-linux-gnueabi/bin/:$PATH

make ARCH=arm CROSS_COMPILE=arm-cortex_a8-linux-gnueabi- mrproper
if [ $? != 0 ]; then
    echo "ERROR: mrproper"
    exit
fi

make ARCH=arm multi_v7_defconfig
if [ $? != 0 ]; then
    echo "ERROR: multi_v7_defconfig"
    exit
fi

make -j4 ARCH=arm CROSS_COMPILE=arm-cortex_a8-linux-gnueabi- zImage
if [ $? != 0 ]; then
    echo "ERROR: zImage"
    exit
fi

make -j4 ARCH=arm CROSS_COMPILE=arm-cortex_a8-linux-gnueabi- modules
if [ $? != 0 ]; then
    echo "ERROR: modules"
    exit
fi

make ARCH=arm CROSS_COMPILE=arm-cortex_a8-linux-gnueabi- dtbs
if [ $? != 0 ]; then
    echo "ERROR: dtbs"
    exit
fi

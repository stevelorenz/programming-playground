#!/bin/bash

make ARCH=arm mrproper
if [ $? != 0 ]; then
    echo "ERROR: mrproper"
    exit
fi

make ARCH=arm versatile_defconfig
if [ $? != 0 ]; then
    echo "ERROR: versatile_defconfig"
    exit
fi

make -j4 ARCH=arm zImage
if [ $? != 0 ]; then
    echo "ERROR: zImage"
    exit
fi

make -j4 ARCH=arm modules
if [ $? != 0 ]; then
    echo "ERROR: modules"
    exit
fi

make ARCH=arm dtbs
if [ $? != 0 ]; then
    echo "ERROR: dtbs"
    exit
fi

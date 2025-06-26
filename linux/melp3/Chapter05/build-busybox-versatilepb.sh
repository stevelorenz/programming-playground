#!/bin/bash

PATH=${HOME}/x-tools/arm-unknown-linux-gnueabi/bin/:$PATH

make ARCH=arm CROSS_COMPILE=arm-unknown-linux-gnueabi-

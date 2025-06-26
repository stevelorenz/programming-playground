# Environment variables used by my tests

export ARCH=arm
export CROSS_COMPILE=arm-linux-gnueabi-
export SYSROOT=$(arm-linux-gnueabi-gcc -print-sysroot)

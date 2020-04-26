#!/bin/bash

if [[ ! -d "$HOME/linux" ]]; then
    echo "Can not find the Linux source directory!"
    exit 1
fi

cp ./syscalls.h ~/linux/include/linux/syscalls.h
cp -r ./hello_world/ ~/linux/
cp ./syscall_64.tbl ~/linux/arch/x86/entry/syscalls/syscall_64.tbl

#!/bin/bash

if [[ -e ./static_memory.out ]]; then
    rm ./static_memory.out
fi

if [[ -e ./dummy_main.out ]]; then
    rm ./dummy_main.out
fi

gcc -o dummy_main.out ./dummy_main.c
echo "* Size output of dummy_main:"
size ./dummy_main.out

gcc -o static_memory.out ./static_memory.c
echo ""
echo "* For static_memory."
echo "-- Size output of static_memory:"
size ./static_memory.out

echo "-- Content of data segment:"
objdump -s -j .data ./static_memory.out

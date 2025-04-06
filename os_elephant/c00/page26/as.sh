#!/bin/bash

nasm -f elf 1.asm -o 1.o
nasm -f elf 2.asm -o 2.o
ld -melf_i386 1.o 2.o -o 12.a
./12.a
echo "==========================================="
readelf -e ./12.a

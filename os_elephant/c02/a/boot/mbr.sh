#!/bin/bash

nasm -o mbr.bin mbr.S && dd if=./mbr.bin of=./hd.img bs=512 count=1  conv=notrunc

#!/bin/bash

if [ -e "hd.img" ]; then
	rm ./hd.img
fi

bximage -q -func=create -hd=60M hd.img

nasm -I include/ -o ./mbr.bin mbr.S && \
	dd if=./mbr.bin of=./hd.img bs=512 count=1 conv=notrunc
nasm -I include/ -o ./loader.bin loader.S && \
	dd if=./loader.bin of=./hd.img bs=512 count=1 seek=2 conv=notrunc

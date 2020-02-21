#!/bin/bash
#
# build.sh
#

for file in ./*.c; do
    filename="${file##*/}"
    base="${filename%.c}"
    gcc $(pkg-config --cflags --libs glib-2.0) "$base.c" -o "$base.out"
done

#!/bin/bash

if [[ ! -e ./infi_loop.out ]]; then
    gcc -o ./infi_loop.out ./infi_loop.c
fi

./infi_loop.out &
PID=$!
echo "Running infi_loop.out with PID:$PID in background."

echo "* Cat its memory mapping:"
cat /proc/$PID/maps

echo "Kill infi_loop.out with PID:$PID."
kill -9 $PID

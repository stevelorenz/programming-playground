#!/bin/bash

gcc -g -c -fsanitize=address -std=c11 ./player.c
gcc -g -c -fsanitize=address -std=c11 ./gun.c
gcc -g -fsanitize=address -std=c11 main.c gun.o player.o

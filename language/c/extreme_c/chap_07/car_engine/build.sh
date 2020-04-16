#!/bin/bash

gcc -g -c -fsanitize=address -std=c11 ./engine.c
gcc -g -c -fsanitize=address -std=c11 ./car.c
gcc -g -fsanitize=address -std=c11 main.c car.o engine.o

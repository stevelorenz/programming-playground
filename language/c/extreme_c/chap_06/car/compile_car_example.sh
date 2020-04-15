#!/bin/bash

gcc -c -fsanitize=address -g -std=c11 ./car.c
gcc -fsanitize=address -std=c11 -g car_main.c car.o

#!/bin/bash

gcc -c -fsanitize=address -g -std=c11 ./list.c
gcc -Wall -fsanitize=address -std=c11 -g ./test_list.c list.o

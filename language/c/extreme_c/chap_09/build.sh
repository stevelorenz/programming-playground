#!/bin/bash
gcc -g -fsanitize=address -Wall -Wextra -Wno-unused-parameter -std=c11 ./interface.c -o interface.out

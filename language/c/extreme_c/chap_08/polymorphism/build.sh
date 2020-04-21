#!/bin/bash

gcc -g -c -fsanitize=address -Wall -Wextra -Wno-unused-parameter -std=c11 ./animal.c
gcc -g -c -fsanitize=address -Wall -Wextra -Wno-unused-parameter -std=c11 ./cat.c
gcc -g -fsanitize=address -Wall -Wextra -Wno-unused-parameter -std=c11 main.c animal.o cat.o

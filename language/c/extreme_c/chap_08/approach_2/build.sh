#!/bin/bash

gcc -g -c -fsanitize=address -Wall -Wextra -Wno-unused-parameter -std=c11 ./person.c
gcc -g -c -fsanitize=address -Wall -Wextra -std=c11 ./student.c
gcc -g -fsanitize=address -Wall -Wextra -std=c11 main.c person.o student.o

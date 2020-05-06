#!/bin/bash

set -x

gcc -Wall -c -g -fPIC cstack.c -o cstack.o
gcc -Wall -shared cstack.o -o libcstack.so

gcc -Wall -c -g cstack_tests.c -o tests.o
gcc -Wall tests.o -lcstack -L./ -o cstack_tests.out

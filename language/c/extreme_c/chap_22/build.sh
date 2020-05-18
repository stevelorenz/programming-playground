#!/bin/bash
gcc -c -g -fsanitize=address -Wall ./rand.c
# Instead of the rand, call the wrapper function defined in ./test_rand.c
gcc -g -fsanitize=address -Wall -lcmocka -Wl,--wrap=rand ./test_rand.c rand.o -o ./test_rand.out

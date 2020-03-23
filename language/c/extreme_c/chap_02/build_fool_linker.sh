#!/bin/bash

rm ./*.o
rm ./*.out
gcc -c ./fool_linker_add.c
gcc -c ./fool_linker_main.c
gcc ./fool_linker_add.o ./fool_linker_main.o -o fool_linker.out

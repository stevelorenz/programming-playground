#!/bin/bash

g++ -g -O2 -o ./fib.out ./fib.cpp

# Get the call graph
perf record --call-graph dwarf ./fib.out

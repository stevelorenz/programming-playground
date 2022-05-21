#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8

"""
About: Simple example to use generator and yield keyword
"""

import time

"""
Have to work with a large dataset that is overwhelmed the machine's memory.
Have a complex function that needs to maintain an internal state every time it's called,
but the function is too small to justify creating its own class.

According to PEP 255, generator functions are special kind of function that return a lazy iterator.

"""


def csv_reader(file_name):
    file = open(file_name)
    # This line will load everything into the memory once, even the open() method returns a generator
    result = file.read().splitlines()
    file.close()
    return result


csv_gen = csv_reader("./some_csv.txt")
row_count = 0

for row in csv_gen:
    row_count += 1

print(f"Row count is {row_count}")


def csv_reader_gen(file_name):
    for row in open(file_name, "r"):
        yield row


csv_gen = csv_reader_gen("./some_csv.txt")
row_count = 0

for row in csv_gen:
    row_count += 1

print(f"Row count is {row_count}")

# This is a generator expression!
csv_gen = (row for row in open("./some_csv.txt"))
row_count = 0

for row in csv_gen:
    row_count += 1

print(f"Row count is {row_count}")


def infinite_sequence():
    num = 0
    while True:
        # Keep the state of the num and return the function at this point
        yield num
        num += 1


# for i in infinite_sequence():
#     print(i, end=" ")
#     time.sleep(0.5)

# The next built-in function is used to call the generator directly
gen = infinite_sequence()
print(next(gen))
print(next(gen))
print(next(gen))

nums_square_lc = [num**2 for num in range(100)]
nums_square_gc = (nums**2 for num in range(100))

print(type(nums_square_gc))

import sys

print(f"Size of the lc: {sys.getsizeof(nums_square_lc)}")
print(f"Size of the gc: {sys.getsizeof(nums_square_gc)}")

import cProfile

cProfile.run("sum([i*2 for i in range(10000)])")
cProfile.run("sum((i*2 for i in range(10000)))")

"""
When the Python yield statement is hit, the program suspends function execution.
When a function is suspended, the state of that function is saved.
This includes any variable bindings local to the generator, the instruction pointer,
the internal stack, and any exception handling.
This allows you to resume function execution whenever you call one of generator's methods.
In this way, all function evaluation picks back up right after yield
"""


def multiple_yield():
    yield_str = "This will print the first string"
    yield yield_str
    yield_str = "This will print the second string"
    yield yield_str


multi_yield_ge = multiple_yield()
print(next(multi_yield_ge))
print(next(multi_yield_ge))
try:
    print(next(multi_yield_ge))
except StopIteration:
    print("StopIteration is detected as expected!")

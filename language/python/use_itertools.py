#!/usr/bin/env python3
# coding=utf-8

import itertools
import operator

# Accumulate
data = [1, 2, 3, 4, 5]
res = itertools.accumulate(data, operator.mul)
res = list(res)
assert res == [1, 2, 6, 24, 120]
res = itertools.accumulate(data)
res = list(res)
assert res == [1, 3, 6, 10, 15]

# Combinations
data = [1, 2, 3]
res = list(itertools.combinations(data, 2))
assert res == [
    (1, 2),
    (1, 3),
    (2, 3),
]
res = list(itertools.combinations_with_replacement(data, 2))
assert res == [(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)]

# Permutations
data = [1, 2, 3]
res = list(itertools.permutations(data, 2))
assert res == [(1, 2), (1, 3), (2, 1), (2, 3), (3, 1), (3, 2)]

num_data = [1, 2, 3]
alpha_data = ["a", "b", "c"]
res = list(itertools.product(num_data, alpha_data))
assert res == [
    (1, "a"),
    (1, "b"),
    (1, "c"),
    (2, "a"),
    (2, "b"),
    (2, "c"),
    (3, "a"),
    (3, "b"),
    (3, "c"),
]

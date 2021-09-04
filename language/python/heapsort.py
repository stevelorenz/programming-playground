#!/usr/bin/env python3
# coding=utf-8

import heapq


def heapsort_min(data):
    h = []
    for d in data:
        heapq.heappush(h, d)
    res = [heapq.heappop(h) for _ in range(len(data))]
    return res


def heapsort_max(data):
    h = []
    for d in data:
        heapq.heappush(h, -(d))
    res = [-(heapq.heappop(h)) for _ in range(len(data))]
    return res


data = [1, 3, 5, 7, 9, 2, 4, 6, 8, 0, -2, -3, -5]
res = heapsort_min(data)
assert res == [-5, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
res = heapsort_max(data)
assert res == [9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -2, -3, -5]

#
# @lc app=leetcode id=149 lang=python3
#
# [149] Max Points on a Line
#

# @lc code=start

import math


def get_slope(dx, dy):
    # When slope is negative -1 / 3 = 1 / -3
    if dx * dy < 0:
        dx = abs(dx)
        dy = -abs(dy)
    # When slope is positive -1/-3 = 1/3
    else:
        dx = abs(dx)
        dy = abs(dy)
    gcd = math.gcd(dx, dy)
    if gcd != 0:
        dx = dx / gcd
        dy = dy / gcd

    return (dx, dy)


class Solution:
    def maxPoints(self, points: List[List[int]]) -> int:
        # map all possible angle
        if len(points) == 0:
            return 0
        ls = len(points)
        max_point = 0
        for i in range(ls):
            line_counter = {}
            overlap = 0
            max_point_i = 0
            for j in range(i + 1, ls):
                dx, dy = points[j][0] - points[i][0], points[j][1] - points[i][1]

                # i and j are overlapped, continue
                if dx == 0 and dy == 0:
                    overlap += 1
                    continue

                dx, dy = get_slope(dx, dy)

                if (dx, dy) not in line_counter:
                    line_counter[(dx, dy)] = 1
                else:
                    line_counter[(dx, dy)] += 1

                max_point_i = max(max_point_i, line_counter[(dx, dy)])

            # Plus 1 for the current point i.
            max_point = max(max_point, max_point_i + overlap + 1)

        return max_point


# @lc code=end

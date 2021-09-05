#
# @lc app=leetcode id=120 lang=python3
#
# [120] Triangle
#

# @lc code=start
class Solution:
    def minimumTotal(self, triangle: List[List[int]]) -> int:
        # This is a common dynammic programming problem
        # The minimum path of i-th row = minimum path of (i-1) row + minimum selection of i-th row

        # Here I use the top-bottom approach
        if triangle is None or len(triangle) == 0:
            return 0

        memo = {}

        def path(triangle, i, j):
            if i == len(triangle):
                return 0

            if (i, j) in memo:
                return memo[(i, j)]

            memo[(i, j)] = (
                min(path(triangle, i + 1, j), path(triangle, i + 1, j + 1))
                + triangle[i][j]
            )

            return memo[(i, j)]

        # f(i, j) = min(f(i+1, j), f(i+1, j+1)) + triangle[i][j]
        return path(triangle, 0, 0)


# @lc code=end

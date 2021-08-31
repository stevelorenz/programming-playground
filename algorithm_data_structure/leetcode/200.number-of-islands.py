#
# @lc app=leetcode id=200 lang=python3
#
# [200] Number of Islands
#

# @lc code=start

from collections import deque


class Solution:
    def numIslands(self, grid: List[List[str]]) -> int:
        # Use BFS
        try:
            r = 0
            m = len(grid)
            n = len(grid[0])
            around = ((0, 1), (1, 0), (0, -1), (-1, 0))
        except:
            return 0

        for i in range(m):
            for j in range(n):
                if int(grid[i][j]):
                    r += 1

                    # Start BFS
                    q = deque()
                    q.append((i, j))
                    while q:
                        x, y = q.popleft()

                        if int(grid[x][y]):
                            grid[x][y] = "0"

                            for a, b in around:
                                a += x
                                b += y
                                if 0 <= a < m and 0 <= b < n and int(grid[a][b]):
                                    q.append((a, b))
        return r


# @lc code=end

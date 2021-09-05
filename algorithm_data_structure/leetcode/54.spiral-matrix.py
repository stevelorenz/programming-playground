#
# @lc app=leetcode id=54 lang=python3
#
# [54] Spiral Matrix
#

# @lc code=start
class Solution:
    def spiralOrder(self, matrix: List[List[int]]) -> List[int]:
        # Do a circle in ont round
        # Handle the numbers in the outest cycle
        row = len(matrix)
        if row == 0 or len(matrix[0]) == 0:
            return []
        col = len(matrix[0])

        res = matrix[0]
        if row > 1:
            # up to bottom
            for i in range(1, row):
                # Put the last element of the column in the result
                res.append(matrix[i][-1])
            # right to left
            for j in range(col - 2, -1, -1):
                res.append(matrix[-1][j])
            # bottom to up
            if col > 1:
                for i in range(row - 2, 0, -1):
                    res.append(matrix[i][0])

        # Create a new matrix of internal data
        M = []
        for k in range(1, row - 1):
            t = matrix[k][1:-1]  # the -1 is not included!
            M.append(t)

        # Recursive call is important!!! Generate a new matrix.
        return res + self.spiralOrder(M)


# @lc code=end

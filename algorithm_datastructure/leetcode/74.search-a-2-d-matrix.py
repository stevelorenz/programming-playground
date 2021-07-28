#
# @lc app=leetcode id=74 lang=python3
#
# [74] Search a 2D Matrix
#

# @lc code=start
class Solution:
    def searchMatrix(self, matrix: List[List[int]], target: int) -> bool:
        # Binary search can be used here since the matrix is already sorted
        ls_row = len(matrix)
        ls_col = len(matrix[0])
        if target < matrix[0][0] or target > matrix[-1][-1]:
            return False
        # Minus 1 is important since it is an indices for a array
        begin, end = 0, ls_row * ls_col - 1
        while begin <= end:
            mid = (begin + end) // 2
            row = mid // ls_col
            col = mid % ls_col
            if matrix[row][col] == target:
                return True
            elif matrix[row][col] > target:
                end = mid - 1
            else:
                begin = mid + 1

        return False


# @lc code=end

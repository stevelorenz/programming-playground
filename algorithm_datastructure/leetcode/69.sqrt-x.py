#
# @lc app=leetcode id=69 lang=python3
#
# [69] Sqrt(x)
#

# @lc code=start
class Solution:
    def mySqrt(self, x: int) -> int:
        if x == 0:
            return 0
        left = 0
        right = x
        sqrt = x
        # Uses left close and right close
        while left <= right:
            mid = (left + right) // 2
            if mid ** 2 == x:
                return mid
            elif mid ** 2 < x:
                left = mid + 1
                sqrt = mid
            else:
                right = mid - 1

        return sqrt


# @lc code=end

#
# @lc app=leetcode id=7 lang=python3
#
# [7] Reverse Integer
#

# @lc code=start
class Solution:
    def reverse(self, x: int) -> int:
        if x == 0:
            return x

        res, sign = 0, 1
        if x < 0:
            sign = -1
            x = abs(x)
        
        while x != 0:
            res = res * 10 + x % 10
            if res > int(2 ** 31 - 1):
                return 0
            x = x // 10

        return res * sign


# @lc code=end

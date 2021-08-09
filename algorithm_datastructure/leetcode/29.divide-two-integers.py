#
# @lc app=leetcode id=29 lang=python3
#
# [29] Divide Two Integers
#

# @lc code=start
class Solution:
    def divide(self, dividend: int, divisor: int) -> int:
        """
        Binary search can be used here to speed up the searching
        """
        if (divisor > 0 and dividend > 0) or (divisor < 0 and dividend < 0):
            flag = 1
        else:
            flag = -1

        dividend = abs(dividend)
        divisor = abs(divisor)

        low, upper = 0, dividend

        while low <= upper:
            mid = (low + upper) // 2

            # The chosen mid is too large
            if dividend < mid * divisor:
                upper = mid - 1
            # The chosen mid is too small
            elif (mid + 1) * divisor <= dividend:
                low = mid + 1
            # Get the result!
            elif (mid) * divisor <= dividend < (mid + 1) * divisor:
                # The result should be limited to the given range
                return min(max(-(2 ** 31), flag * mid), 2 ** 31 - 1)


# @lc code=end

#
# @lc app=leetcode id=123 lang=python3
#
# [123] Best Time to Buy and Sell Stock III
#

# @lc code=start
class Solution:
    def maxProfit(self, prices: List[int]) -> int:
        # Now! There's constraints -> dynamic programming
        size = len(prices)
        if size <= 1:
            return 0

        b1 = b2 = -prices[0]
        s1 = s2 = 0

        return max(s1, s2)


# @lc code=end
